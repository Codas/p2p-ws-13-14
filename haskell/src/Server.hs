module Main where

import           Control.Applicative
import           Control.Concurrent         (forkIO, killThread, threadDelay)
import           Control.Concurrent.Chan    (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar    (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM     (TVar, atomically, modifyTVar',
                                             newTVarIO, readTVarIO)
import           Control.Exception          (SomeException, handle)
import           Control.Lens               hiding (argument)
import           Control.Lens.Setter        hiding (argument)
import           Control.Monad
import           Control.Monad.Trans        (liftIO)
import           Options.Applicative        hiding ((&))
import           System.Timeout

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LS
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List
import           Data.Maybe
import qualified Data.Text                  as Text
import           Network.Socket             (ShutdownCmd (ShutdownSend), Socket,
                                             sClose, shutdown, socketToHandle)
import           Prelude
import           System.IO
import           System.Random              (randomRIO)
import qualified Text.Read                  as R

import qualified System.Console.Haskeline   as Sig

import qualified P2P.Marshalling            as M
import           P2P.Messages
import qualified P2P.Networking             as Net
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

type NodeChan = Chan (Message, (Socket, Handle))

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
        when (null joinLocations) $ void $ newNode nodeGen stdout chansT
        when (opBroadcast options) $ do
            forkIO $ sendContentMessages serverID (lSock, stdout) chansT
            return ()
        forever . Net.acceptFork lSock $ \(rSock, rHandle, rAddr) -> do
            putStrLn $ "[Conn] accepted new connection: " ++ show rAddr
            chans <- readTVarIO chansT
            chan <- pick chans
            handleToMessages chan (rSock, rHandle) (Just chansT)

handleInterrupt :: Sig.InputT IO () -> IO ()
handleInterrupt f = Sig.runInputT Sig.defaultSettings $ Sig.withInterrupt
    $ Sig.handleInterrupt (liftIO initShutdown)
    $ f

initShutdown :: IO ()
initShutdown = do
    putStrLn "!SIGINT!"

joinCircle nodeGen lSock chansT port joinLocation = do
    (node, chan) <- newNode nodeGen lSock chansT
    let jAddr        = joinAddr joinLocation
        jPort        = joinPort joinLocation
        split (s, h) = putStrLn "[Action] Splitting initiated" >> sendMessage h splitMsg
        splitMsg     = SplitEdgeMessage "127.0.0.1" port (_location node)
        loc          = _location node
    connectAndHandleSafe jAddr jPort (loc, chan) split
    return (node, chan)

newNode :: IO Node -> Handle -> TVar [(Location, NodeChan)] -> IO (Node, NodeChan)
newNode nodeGen lSock chansT = do
    node <- nodeGen
    chan <- newChan
    atomically $ modifyTVar' chansT ((_location node, chan):)
    putStrLn $ "[State] created new node: " ++ show node
    forkIO $ handleNode node chan chansT
    return (node, chan)

sendContentMessages :: NodeID -> (Socket, Handle) -> TVar [(Location, NodeChan)] -> IO ()
sendContentMessages serverID acc chansT = do
    chans <- readTVarIO chansT
    (_, chan) <- pick chans
    threadDelay 2000000
    writeChan chan (SendContentMessage serverID (Text.pack "hallo leute!"), acc)
    threadDelay 2000000
    -- unless (null chans) $ sendContentMessages serverID lSock chansT
    return ()

handleToMessages :: (Location, NodeChan) -> (Socket, Handle)
                 -> Maybe (TVar [(Location, NodeChan)]) -> IO ()
handleToMessages (loc, chan) acc@(rSock, rHandle) mChansT = do
    handle handleError convert
    putStrLn "[Conn State] DONE!"
  where handleError :: SomeException -> IO ()
        handleError e = do
            putStrLn $ "[Exception] " ++ show e
            writeChan chan (Shutdown, acc)
        locChan mLoc cs = do
            chans <- readTVarIO cs
            return $ head $ filter (\(l, _) -> l == mLoc) chans
        convert = do
            bytes <- LS.hGetContents rHandle
            case mChansT of
                Just chansT -> do
                    let mMLoc = case M.byteStringToMessage bytes of
                                  (Just (HelloCWMessage _ l), _) -> Just l
                                  (Just (HelloCCWMessage _ l), _) -> Just l
                                  _ -> Nothing
                    case mMLoc of
                        Just mLoc -> do
                            (_, nChan) <- locChan mLoc chansT
                            bytesToMessages nChan (rSock, rHandle) bytes
                        _ -> bytesToMessages chan (rSock, rHandle) bytes
                _ -> bytesToMessages chan (rSock, rHandle) bytes

-- Just read every command
bytesToMessages :: NodeChan -> (Socket, Handle) -> LS.ByteString -> IO ()
bytesToMessages chan acc@(rSock, rHandle) bs
    | LS.null bs = putStrLn "[Conn] Disconnected" >>
                   writeChan chan (Shutdown, (rSock, rHandle))
    | otherwise  =
        case M.byteStringToMessage bs of
            (Just msg, rest) -> do
                putStrLn $ "[Message] new message: " ++ show msg
                writeChan chan (msg, acc)
                bytesToMessages chan acc rest
            (_, rest) -> do
                putStrLn "[Message] got 'something'..."
                bytesToMessages chan acc rest

handleNode :: Node -> Chan (Message, (Socket, Handle)) -> TVar [(Location, NodeChan)] -> IO ()
handleNode self chan chansT
    | isDone self = putStrLn "[State] Node done!" >>
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
answer :: Message -> Node -> (Socket, Handle) -> (Location, NodeChan) -> IO Node
answer Shutdown node (rSock, rHandle) _
    | _state node == Merging = do
        putStrLn $ "[Handling] Shutdown. Merging -> Done. " ++ show rHandle
        when (isStarved node) $ forAllSockets_ node closeSafeS
        closeSafeS rSock >> return (deletePeer node rSock & protocolState .~ Done)
    | _state node == Splitting = do
        putStrLn $ "[Handling] Shutdown. Splitting -> Close. " ++ show rHandle
        putStrLn ".   .   .   .   ."
        print node
        putStrLn ".   .   .   .   ."
        let freeNode = node & protocolState .~ Free
        case _otherPeer node of
            Nothing -> return freeNode
            _       -> return $ freeNode & otherPeer .~ Nothing & cwPeer .~ _otherPeer node
    | otherwise = putStrLn ("[Handling] Shutdown. Closing. " ++ show rSock )>>
                  closeSafeS rSock >> return (deletePeer node rSock)
  where deletePeer :: Node -> Socket -> Node
        deletePeer node peer
            | nodeSocket node _cwPeer    == Just peer = node & cwPeer    .~ Nothing
            | nodeSocket node _ccwPeer   == Just peer = node & ccwPeer   .~ Nothing
            | nodeSocket node _otherPeer == Just peer = node & otherPeer .~ Nothing
            | otherwise                               = node

answer (SplitEdgeMessage rAddr rPort rLoc) node acc@(rSock, rHandle) chan
    | isBusy node              = putStrLn "[Handling] SplitEdge. Is Busy!" >>
                                 closeNode node acc
    | isJust (_otherPeer node) = putStrLn "[Handling] SplitEdge. Has other Peer!" >>
                                 closeNode node acc
    | isNothing (_cwPeer node) = do
        putStrLn "[Handling] SplitEdge. No Peers."
        forkIO $ sendMessage rHandle helloCCW
        Just (pSock, pHandle) <- connectAndHandleSafe rAddr rPort chan helloCW
        return $ node & ccwPeer .~ Just (Peer pHandle pSock True rLoc) & cwPeer .~ newPeer
    | otherwise = do
        putStrLn "[Handling] SplitEdge. Normal Operation."
        _ <- forkIO $ sendMessage rHandle helloCCW
        _ <- forkIO $ sendMessage (_handle (fromJust (_cwPeer node))) redirect
        return newNode
  where lLoc           = _location node
        newNode        = node & otherPeer .~ newPeer & protocolState .~ Splitting
        helloCCW       = HelloCCWMessage lLoc rLoc
        helloCW (s, h) = sendMessage h $ HelloCWMessage (_location node) rLoc
        redirect       = RedirectMessage rAddr rPort rLoc
        newPeer        = Just $ Peer rHandle rSock True rLoc


answer (HelloCCWMessage srcLoc trgLoc) node acc@(rSock, rHandle) _
    | trgLoc /= _location node = putStrLn "[Handling] HelloCCW. Location mismatch (CCW)." >>
                                 closeNode node acc
    | otherwise = putStrLn "[Handling] HelloCCW. All good." >>
                  return newNode
  where newNode = node & ccwPeer .~ peer & otherPeer .~ Nothing
        peer = Just $ Peer rHandle rSock True srcLoc

-- HelloCW always denotes the end of the protocol (end of a join), so we
-- can set the new peers and mark the node (ourselfes) as Free.
-- We have now successfully joined the network!
answer (HelloCWMessage srcLoc trgLoc) node acc@(rSock, rHandle) _
    | trgLoc /= _location node = putStrLn "[Handling] HelloCW. Location mismatch." >>
                                 closeNode node acc
    | otherwise = putStrLn "[Handling] HelloCW. All good." >>
                  return (newNode & protocolState .~ Free)
  where newNode = node & cwPeer .~ peer & otherPeer .~ Nothing
        peer = Just $ Peer rHandle rSock True srcLoc

answer (RedirectMessage addr port trgLoc) node acc@(rSock, rHandle) chan
    | isBusy node                                 = putStrLn "[Handling] Redirect. Is Busy. Canceling." >>
                                                    cancel
    | Just rSock /= fmap _socket (_ccwPeer node) = putStrLn "[Handling] Redirect. Invalid peer!. Canceling." >>
                                                   cancel
    | otherwise = do
        putStrLn "[Handling] Redirect. All good"
        putStrLn "greeting new ccw"
        status <- connectAndHandleSafe addr port chan hello
        case status of
            Just pSock -> handleSuccess pSock
            _          -> cancel >> return node
  where hello (s, h) = sendMessage h $ HelloCWMessage (_location node) trgLoc
        cancel       = sendMessage rHandle CancelMessage >> closeNode node acc
        handleSuccess (pS, pH) = do
            putStrLn "[Action] shutting down connection do old ccw peer"
            mapM_ closeSafeS $ maybeToList $ nodeSocket node _ccwPeer
            return $ node & otherPeer .~ _ccwPeer node & ccwPeer .~ Just newPeer
          where newPeer = Peer pH pS True trgLoc

answer CancelMessage node rSock _ = do
        putStrLn $ "[Handling] Cancel. All good. Shutting down Send. " ++ show rSock
        print node
        mapM_ closeSafeS $ maybeToList $ nodeSocket node _otherPeer
        return $ node & otherPeer .~ Nothing

answer (MergeEdgeMessage addr port trgLoc) node acc@(rSock, rHandle) chan
    | isBusy node                                = putStrLn "[Handling] MergeEdge. Is Busy. TryLater." >>
                                                   cancel >> return node
    | Just rSock /= fmap _socket (_cwPeer node)  = putStrLn "[Handling] MergeEdge. Invalid peer!. Canceling." >>
                                                   cancel >> return node
    | otherwise = do
        putStrLn "[Handling] MergeEdge. All good"
        putStrLn "greeting new cw"
        status <- connectAndHandleSafe addr port chan hello
        case status of
            Just pSock -> handleSuccess acc
            _          -> cancel >> return node
  where hello (s, h) = sendMessage h $ HelloCCWMessage (_location node) trgLoc
        cancel       = sendMessage rHandle (TryLaterMessage)
        handleSuccess (s, h) = do
            putStrLn "[Action] shutting down connection to old cw peer"
            mapM_ closeSafeS $ maybeToList $ nodeSocket node _cwPeer
            return $ node & otherPeer .~ _cwPeer node & cwPeer .~ Just newPeer
          where newPeer = Peer h s True trgLoc

answer TryLaterMessage node (rSock, rHandle) _ = return node
answer msg@(ContentMessage srcNodeID srcLoc content) node _ _ = do
   putStrLn "[Handling] Content. All Good."
   putStrLn $ "[CONTENT] " ++ show content -- debugging only
   when ((nodeID, loc) /= (srcNodeID, srcLoc)) $ do
       threadDelay 1000000
       putStrLn $ "[Handling] Forwarding content message. To cw peer: " ++ show (_cwPeer node)
       mapM_ (`sendMessage` msg) cwHandle
       return ()
   when ((nodeID, loc) == (srcNodeID, srcLoc)) $ putStrLn "[Handling] Not sending content message"
   return node
  where cwHandle  = maybeToList $ nodeHandle node _cwPeer
        nodeID    = _nodeID node
        loc       = _location node

answer (SendContentMessage nodeID content) node _ _ = do
    putStrLn "[Handling] send content message"
    mapM_ (`sendMessage` msg) $ maybeToList handle
    return node
  where msg = ContentMessage nodeID loc content
        handle = fmap _handle $ _cwPeer node
        loc = _location node

sendMessage :: Handle -> Message -> IO ()
sendMessage rHandle msg = putStrLn ("[Action] Sending Message: " ++ show msg) >>
                        BS.hPut rHandle (M.messageToByteString msg)

closeSafeS sock = do
    putStrLn "[Conn] closing connection (socket)..."
    handle logError $ sClose sock
    putStrLn "[Conn] connection closed."

closeSafeH handle = do
    putStrLn "[Conn] closing connection (handle)..."
    hFlush handle
    hClose handle
    putStrLn "[Conn] connection closed."

closeNode :: Node -> (Socket, Handle) -> IO Node
closeNode node acc@(rSock, rHandle) = do
    (putStrLn $ "[Action] Close Node and Socket! " ++ show rHandle)
    (handle logError $ closeSafeS rSock)
    return (node & otherPeer .~ Nothing)

logError :: SomeException -> IO ()
logError e = putStrLn $ "[LOG] " ++ show e

------------------------------------------
-- Async safe connections with timeouts --
------------------------------------------

connectAndHandleSafe :: String -> String -> (Location, NodeChan)
                     -> ((Socket, Handle) -> IO ()) -> IO (Maybe (Socket, Handle))
connectAndHandleSafe addr port chan action = do
    mvar <- newEmptyMVar
    let handleError :: SomeException -> IO ()
        handleError e = print e >> putMVar mvar Nothing
    tid <- forkIO $ handle handleError $ connect mvar
    success <- timeout 2000000 $ takeMVar mvar
    case success of
         Just (Just acc) -> (action (acc)) >> return (Just acc)
         _               -> killThread tid >> putStrLn failedMsg >> return Nothing
  where failedMsg = "failed to connect to: " ++ addr ++ ":" ++ port
        connect mvar = Net.connectTo addr port $ \(sock, handle, rAddr) -> do
            putStrLn ("connected to: " ++ show rAddr)
            putMVar mvar (Just (sock, handle))
            handleToMessages chan (sock, handle) Nothing

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
