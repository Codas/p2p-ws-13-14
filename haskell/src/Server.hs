module Main where

import           Control.Applicative
import           Control.Concurrent             (forkIO, killThread,
                                                 threadDelay)
import           Control.Concurrent.Chan        (Chan, newChan, readChan,
                                                 writeChan)
import           Control.Concurrent.MVar        (newEmptyMVar, putMVar,
                                                 takeMVar)
import           Control.Concurrent.STM         (TVar, atomically, modifyTVar',
                                                 newTVarIO, readTVarIO)
import           Control.Exception              (SomeException, handle)
import           Control.Lens                   hiding (argument)
import           Control.Lens.Setter            hiding (argument)
import           Control.Monad
import           Control.Monad.Trans            (liftIO)
import           Options.Applicative            hiding ((&))
import           System.Timeout

import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy           as LS
import           Data.List
import           Data.Maybe
import qualified Data.Text                      as Text
import           Network.Socket                 (ShutdownCmd (ShutdownSend),
                                                 Socket, sClose, shutdown,
                                                 socketToHandle)
import qualified Network.Socket.ByteString      as NBS
import qualified Network.Socket.ByteString.Lazy as NLS
import           Prelude
import           System.IO
import           System.Random                  (randomRIO)
import qualified Text.Read                      as R

import qualified System.Console.Haskeline       as Sig

import qualified P2P.Marshalling                as M
import           P2P.Messages
import qualified P2P.Networking                 as Net
import           P2P.Nodes

-----------------------------------
-- Command line argument parsing --
-----------------------------------
data Opts = Opts
  { opPort      :: String
  , opAddress   :: Net.HostPreference
  , opJoins     :: [JoinLocation]
  , opBroadcast :: Bool
  , opGui       :: Bool}
  deriving Show

-- define possible command line arguments. Call this programm with --help
-- to get a description of every argument
serverOpts :: Parser Opts
serverOpts = Opts
     <$> strOption
         ( long "port"
        <> short 'p'
        <> value "1337"
        <> metavar "PORT"
        <> showDefault
        <> help "Port on which to listen to incoming connections." )
     <*> nullOption
         ( long "address"
        <> short 'a'
        <> value Net.HostAny
        <> metavar "RELAY"
        <> reader hostReader
        <> showDefaultWith (const "Any address")
        <> help "Address to accept incoming connections from. E.g. localhost.")
     <*> many (argument joinReader (metavar "TARGET"))
     <*> switch
         ( long "broadcast"
        <> short 'b'
        <> help "Enable broadcasting to periodically get an overview of the system." )
     <*> switch
         ( long "gui"
        <> short 'g'
        <> help "Enable web based graphical user interface. Binds do port 10000." )

-- custom reader to parse the host preferencec
hostReader :: Monad m => String -> m Net.HostPreference
hostReader s = return $ Net.Host s

joinReader :: Monad m => String -> m JoinLocation
joinReader s = return $ JoinLocation addr port
  where (addr, x:port) = break (== ':') s

data JoinLocation = JoinLocation
                    { joinAddr :: String
                    , joinPort :: String }
                    deriving (Show)

-- build the command line options, including helper text and parser
opts :: ParserInfo Opts
opts = info (serverOpts <**> helper)
  ( fullDesc
 <> progDesc "Start a TCP server and listen to incomming connections"
 <> header "Server - one Server to bring them all and to the socket bind them" )

type NodeChan = Chan (Message, Socket)

pick :: [a] -> IO a
pick xs =  liftM (xs !!) (randomRIO (0, length xs - 1))

--------------------------------------------------------------
-- Main server logic. Listen, accept, send events and pring --
--------------------------------------------------------------

main :: IO ()
main = Net.withSocketsDo $ do
    hSetBuffering stdout LineBuffering
    options  <- execParser opts
    serverID <- newServerID
    nodeGen  <- newNodeGenerator serverID
    chansT   <- newTVarIO ([] :: [(Location, NodeChan)])

    -- forkIO $ handleInterrupt $ forever $ Sig.getInputLine ""

    let addr          = opAddress options
        port          = opPort options
        joinLocations = opJoins options
    Net.listen addr port $ \(lSock, lAddr) -> do
        putStrLn $ "[Conn] listening on: " ++ show lAddr
        mapM_ (joinCircle nodeGen stdout chansT port) joinLocations
        when (null joinLocations) $ void $ newNode nodeGen chansT Free
        when (opBroadcast options) $ do
            forkIO $ sendContentMessages serverID lSock chansT
            return ()
        forever . Net.acceptFork lSock $ \(rSock, rAddr) -> do
            putStrLn $ "[Conn] accepted new connection: " ++ show rAddr
            chans <- readTVarIO chansT
            chan <- pick chans
            socketToMessages chan rSock (Just chansT)

handleInterrupt :: Sig.InputT IO () -> IO ()
handleInterrupt f = Sig.runInputT Sig.defaultSettings $ Sig.withInterrupt
    $ Sig.handleInterrupt (liftIO initShutdown)
    $ f

initShutdown :: IO ()
initShutdown = do
    putStrLn "!SIGINT!"

joinCircle nodeGen lSock chansT port joinLocation = do
    (node, chan) <- newNode nodeGen chansT Joining
    let jAddr      = joinAddr joinLocation
        jPort      = joinPort joinLocation
        split sock = putStrLn "[Action] Splitting initiated" >> sendMessage sock splitMsg
        splitMsg   = SplitEdgeMessage "127.0.0.1" port (_location node)
        loc        = _location node
    forkIO $ void $ connectAndHandleSafe jAddr jPort (loc, chan) split
    return (node, chan)

newNode :: (ProtocolState -> IO Node ) -> TVar [(Location, NodeChan)] -> ProtocolState -> IO (Node, NodeChan)
newNode nodeGen chansT state = do
    node <- nodeGen state
    chan <- newChan
    atomically $ modifyTVar' chansT ((_location node, chan):)
    putStrLn $ "[State] created new node: " ++ show node
    forkIO $ handleNode node chan chansT
    return (node, chan)

sendContentMessages :: NodeID -> Socket -> TVar [(Location, NodeChan)] -> IO ()
sendContentMessages serverID sock chansT = do
    chans <- readTVarIO chansT
    (_, chan) <- pick chans
    threadDelay 1000000
    writeChan chan (SendContentMessage serverID (C8.pack "hallo leute!"), sock)
    threadDelay 5000000
    unless (null chans) $ sendContentMessages serverID sock chansT
    return ()

socketToMessages :: (Location, NodeChan) -> Socket
                 -> Maybe (TVar [(Location, NodeChan)]) -> IO ()
socketToMessages (loc, chan) rSock mChansT = do
    handle handleError convert
  where handleError :: SomeException -> IO ()
        handleError e = do
            -- putStrLn $ "[Exception] " ++ show e
            writeChan chan (ShutdownMessage, rSock)
        locChan mLoc cs = do
            chans <- readTVarIO cs
            return $ head $ filter (\(l, _) -> l == mLoc) chans
        convert = do
            bytes <- NLS.getContents rSock
            case mChansT of
                Just chansT -> do
                    let mMLoc = case M.byteStringToMessage bytes of
                                  (Just (HelloCWMessage _ l), _) -> Just l
                                  (Just (HelloCCWMessage _ l), _) -> Just l
                                  _ -> Nothing
                    case mMLoc of
                        Just mLoc -> do
                            (_, nChan) <- locChan mLoc chansT
                            bytesToMessages nChan rSock bytes
                        _ -> bytesToMessages chan rSock bytes
                _ -> bytesToMessages chan rSock bytes

-- Just read every command
bytesToMessages :: NodeChan -> Socket -> LS.ByteString -> IO ()
bytesToMessages chan rSock bs
    | LS.null bs = putStrLn "[Conn] Disconnected" >>
                   writeChan chan (ShutdownMessage, rSock)
    | otherwise  =
        case M.byteStringToMessage bs of
            (Just msg, rest) -> do
                -- putStrLn $ "[Message] new message: " ++ show msg
                writeChan chan (msg, rSock)
                bytesToMessages chan rSock rest
            (_, rest) -> do
                putStrLn "[Message] got 'something'..."
                bytesToMessages chan rSock rest

handleNode :: Node -> Chan (Message, Socket) -> TVar [(Location, NodeChan)] -> IO ()
handleNode self chan chansT
    | isDone self = putStrLn ("[State] Node done! " ++ show (_location self)) >>
                    atomically (modifyTVar' chansT (delete (loc, chan)))
    | otherwise = do
        putStrLn "-------------------------------"
        print self
        putStrLn ". . . . . . . . . . . . . . . ."
        (msg, rSock) <- readChan chan
        answer msg self rSock (loc, chan) >>= recurse
  where recurse node = handleNode node chan chansT
        loc = _location self

-- Some peer just disconnected. Check if it is of intereset for us, conditionally
-- update the node record, than continue
answer :: Message -> Node -> Socket -> (Location, NodeChan) -> IO Node
answer ShutdownMessage node rSock _
    | _state node == Joining = do
        putStrLn $ "[Handling] Shutdown. Joining failed. " ++ show (_location node)
        return $ node & protocolState .~ Done
    | _state node == Merging = do
        putStrLn $ "[Handling] Shutdown. Merging -> Done. " ++ show (_location node) ++ " - " ++ show rSock
        when (isStarved node) $ forAllSockets_ node closeSafe
        closeSafe rSock >> return (deletePeer node rSock & protocolState .~ Done)
    | _state node == Splitting = do
        putStrLn $ "[Handling] Shutdown. Splitting -> Close. " ++ show (_location node) ++ " - " ++ show rSock
        closeSafe rSock
        let freeNode = node & protocolState .~ Free
        case _otherPeer node of
            Nothing -> return freeNode
            _       -> return $ freeNode & otherPeer .~ Nothing & cwPeer .~ _otherPeer node
    | otherwise = putStrLn ("[Handling] Shutdown. Closing. " ++ show (_location node) ++ " - " ++ show rSock )>>
                  closeSafe rSock >> return (deletePeer node rSock)
  where deletePeer :: Node -> Socket -> Node
        deletePeer node peer
            | nodeSocket node _cwPeer    == Just peer = node & cwPeer    .~ Nothing
            | nodeSocket node _ccwPeer   == Just peer = node & ccwPeer   .~ Nothing
            | nodeSocket node _otherPeer == Just peer = node & otherPeer .~ Nothing
            | otherwise                               = node

answer (SplitEdgeMessage rAddr rPort rLoc) node rSock  chan
    | isBusy node              = putStrLn ("[Handling] SplitEdge. Is Busy! " ++ show (_location node) )>>
                                 closeNode node rSock
    | isJust (_otherPeer node) = putStrLn ("[Handling] SplitEdge. Has other Peer! " ++ show (_location node))  >>
                                 closeNode node rSock
    | isNothing (_cwPeer node) = do
        putStrLn $ "[Handling] SplitEdge. No Peers. " ++ show (_location node)
        forkIO $ sendMessage rSock helloCCW
        Just pSock <- connectAndHandleSafe rAddr rPort chan helloCW
        return $ node & ccwPeer .~ Just (Peer pSock True rLoc) & cwPeer .~ newPeer
    | otherwise = do
        putStrLn $ "[Handling] SplitEdge. Normal Operation. " ++ show (_location node)
        _ <- forkIO $ sendMessage rSock helloCCW
        _ <- forkIO $ sendMessage (_socket (fromJust (_cwPeer node))) redirect
        return newNode
  where lLoc         = _location node
        newNode      = node & otherPeer .~ newPeer & protocolState .~ Splitting
        helloCCW     = HelloCCWMessage lLoc rLoc
        helloCW sock = sendMessage sock $ HelloCWMessage (_location node) rLoc
        redirect     = RedirectMessage rAddr rPort rLoc
        newPeer      = Just $ Peer rSock True rLoc


answer (HelloCCWMessage srcLoc trgLoc) node rSock _
    | trgLoc /= _location node = putStrLn ("[Handling] HelloCCW. Location mismatch (CCW). "  ++ show (_location node) )>>
                                 closeNode node rSock
    | otherwise = putStrLn ("[Handling] HelloCCW. All good." ++ show (_location node) ) >>
                  return newNode
  where newNode = node & ccwPeer .~ peer & otherPeer .~ Nothing
        peer = Just $ Peer rSock True srcLoc

-- HelloCW always denotes the end of the protocol (end of a join), so we
-- can set the new peers and mark the node (ourselfes) as Free.
-- We have now successfully joined the network!
answer (HelloCWMessage srcLoc trgLoc) node rSock _
    | trgLoc /= _location node = putStrLn "[Handling] HelloCW. Location mismatch." >>
                                 closeNode node rSock
    | otherwise = putStrLn ("[Handling] HelloCW. All good." ++ show (_location node)) >>
                  return (newNode & protocolState .~ Free)
  where newNode = node & cwPeer .~ peer & otherPeer .~ Nothing
        peer = Just $ Peer rSock True srcLoc

answer (RedirectMessage addr port trgLoc) node rSock chan
    | isBusy node = putStrLn ("[Handling] Redirect. Is Busy. Canceling." ++ show (_location node)) >>
                    cancel
    | Just rSock /= fmap _socket (_ccwPeer node) = putStrLn "[Handling] Redirect. Invalid peer!. Canceling." >>
                                                   cancel
    | otherwise = do
        putStrLn $ "[Handling] Redirect. All good. " ++ show (_location node)
        putStrLn $ "[Action] greeting new ccw" ++ show (_location node)
        status <- connectAndHandleSafe addr port chan hello
        case status of
            Just pSock -> handleSuccess pSock
            _          -> cancel >> return node
  where hello sock = sendMessage sock $ HelloCWMessage (_location node) trgLoc
        cancel     = sendMessage rSock CancelMessage >> closeNode node rSock
        handleSuccess sock = do
            putStrLn "[Action] shutting down connection to old ccw peer"
            mapM_ closeSafe $ maybeToList $ nodeSocket node _ccwPeer
            return $ node & otherPeer .~ _ccwPeer node & ccwPeer .~ Just newPeer
          where newPeer = Peer sock True trgLoc

answer CancelMessage node rSock _ = do
        putStrLn $ "[Handling] Cancel. All good. Shutting down Send. " ++ show rSock ++ show (_location node)
        print node
        mapM_ closeSafe $ maybeToList $ nodeSocket node _otherPeer
        return $ node & otherPeer .~ Nothing

answer (MergeEdgeMessage addr port trgLoc) node rSock chan
    | isBusy node = putStrLn ("[Handling] MergeEdge. Is Busy. TryLater." ++ show (_location node) )>>
                                                   cancel >> return node
    | Just rSock /= fmap _socket (_cwPeer node)  = putStrLn "[Handling] MergeEdge. Invalid peer!. Canceling." >>
                                                   cancel >> return node
    | otherwise = do
        putStrLn $ "[Handling] MergeEdge. All good" ++ show (_location node)
        putStrLn $ "[Action] greeting new cw" ++ show (_location node)
        status <- connectAndHandleSafe addr port chan hello
        case status of
            Just pSock -> handleSuccess rSock
            _          -> cancel >> return node
  where hello sock = sendMessage sock $ HelloCCWMessage (_location node) trgLoc
        cancel       = sendMessage rSock TryLaterMessage
        handleSuccess sock = do
            putStrLn "[Action] shutting down connection to old cw peer"
            mapM_ closeSafe $ maybeToList $ nodeSocket node _cwPeer
            return $ node & otherPeer .~ _cwPeer node & cwPeer .~ Just newPeer
          where newPeer = Peer sock True trgLoc

answer TryLaterMessage node rSock _ = return node
answer msg@(ContentMessage srcNodeID srcLoc content) node _ _ = do
   when ((nodeID, loc) /= (srcNodeID, srcLoc)) $ do
       putStrLn $ "[Handling] Forwarding content message. To cw peer: " ++ show (_cwPeer node) ++ " from " ++ show (_location node)
       mapM_ (`sendMessage` msg) cwSocket
       return ()
   when ((nodeID, loc) == (srcNodeID, srcLoc)) $
        putStrLn $ "[Handling] Content message Back!" ++ show (_location node)
   return node
  where cwSocket  = maybeToList $ nodeSocket node _cwPeer
        nodeID    = _nodeID node
        loc       = _location node

answer (SendContentMessage nodeID content) node _ _ = do
    putStrLn "[Handling] send content message"
    mapM_ (`sendMessage` msg) $ maybeToList handle
    return node
  where msg = ContentMessage nodeID loc content
        handle = fmap _socket $ _cwPeer node
        loc = _location node

sendMessage :: Socket -> Message -> IO ()
sendMessage rSock msg = putStrLn ("[Action] Sending Message: " ++ show msg ++ " to Socket " ++ show rSock) >>
                        NBS.sendAll rSock (M.messageToByteString msg)

closeSafe sock = do
    putStrLn $ "[Conn] closing connection (socket)..." ++ show sock
    -- threadDelay 500000
    -- sendMessageS sock ShutdownMessage
    handle logError $ sClose sock
    putStrLn "[Conn] connection closed."

closeNode :: Node -> Socket -> IO Node
closeNode node rSock = do
    (putStrLn $ "[Action] Close Node and Socket! " ++ show rSock)
    (handle logError $ closeSafe rSock)
    return (node & otherPeer .~ Nothing)

logError :: SomeException -> IO ()
logError e = putStrLn $ "[LOG] " ++ show e

------------------------------------------
-- Async safe connections with timeouts --
------------------------------------------

connectAndHandleSafe :: String -> String -> (Location, NodeChan)
                     -> (Socket -> IO ()) -> IO (Maybe Socket)
connectAndHandleSafe addr port chan action = do
    mvar <- newEmptyMVar
    let handleError :: SomeException -> IO ()
        handleError e = print e >> putMVar mvar Nothing
    tid <- forkIO $ handle handleError $ connect mvar
    success <- timeout 2000000 $ takeMVar mvar
    case success of
         Just (Just rSock) -> action rSock >> return (Just rSock)
         _               -> killThread tid >> putStrLn failedMsg >> return Nothing
  where failedMsg = "failed to connect to: " ++ addr ++ ":" ++ port
        connect mvar = Net.connectTo addr port $ \(sock, rAddr) -> do
            putStrLn ("[Conn] connected to: " ++ show rAddr)
            putMVar mvar (Just sock)
            socketToMessages chan sock Nothing

-------------------------------
-- General UI initialization --
-------------------------------
-- initUIs :: Evt.NetEventGetter -> Behavior (Set Evt.ClientCon) -> Opts -> IO ()
-- initUIs netEvent clientB options = do
--     when ( opGui options ) $ GUI.init (opGuiPort options) netEvent clientB
--     when (opConsole options) $ initConsoleUI netEvent

----------------
-- Console UI --
----------------
-- initConsoleUI :: Evt.NetEventGetter -> IO ()
-- initConsoleUI netEvent = do
--   evtB <- stepper undefined (netEvent Evt.AnyEvent)
--   onChange evtB print
