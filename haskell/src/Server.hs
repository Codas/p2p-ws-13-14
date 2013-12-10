module Main where

import           Control.Applicative
import           Control.Concurrent             (forkIO, killThread,
                                                 threadDelay)
import           Control.Concurrent.Chan        (Chan, newChan, readChan,
                                                 writeChan)
import           Control.Concurrent.MVar        (MVar, newEmptyMVar, putMVar,
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
import           Data.Word
import           Network.Socket                 (ShutdownCmd (ShutdownSend),
                                                 Socket, sClose, shutdown,
                                                 socketToHandle)
import qualified Network.Socket.ByteString      as NBS
import qualified Network.Socket.ByteString.Lazy as NLS
import           Prelude
import           System.IO
import           System.Random                  (randomRIO)
import           Text.Printf
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
  , opAddress   :: String
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
     <*> strOption
         ( long "address"
        <> short 'a'
        <> value "127.0.0.1"
        <> metavar "ADDRESS"
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
    hSetBuffering stdin  NoBuffering
    options  <- execParser opts
    serverID <- newServerID
    chansT   <- newTVarIO ([] :: [(Location, NodeChan)])
    quitNow  <- newEmptyMVar
    let addr          = opAddress options
        port          = opPort options
        joinLocations = opJoins options
    nodeGen  <- newNodeGenerator serverID addr port
    forkIO $ Net.listen (Net.Host addr) port $ \(lSock, lAddr) -> do
        forkIO $ handleInterrupt chansT lSock $ forever $ Sig.getInputLine ""
        putStrLn $ "[Conn] listening on: " ++ show lAddr
        mapM_ (joinCircle nodeGen stdout chansT port quitNow) joinLocations
        when (null joinLocations) $ void $ newNode nodeGen chansT quitNow Free
        when (opBroadcast options) $ do
            putStrLn "[Action] Starting to send broadcasts..."
            forkIO $ sendContentMessages serverID lSock chansT
            return ()
        forever . Net.acceptFork lSock $ \(rSock, rAddr) -> do
            putStrLn $ "[Conn] accepted new connection: " ++ show rAddr
            chans <- readTVarIO chansT
            chan <- pick chans
            socketToMessages chan rSock (Just chansT)
    takeMVar quitNow
    putStrLn "Bye Bye"
    return ()

handleInterrupt :: TVar [(Location, NodeChan)] -> Socket -> Sig.InputT IO () -> IO ()
handleInterrupt chansT lSock  f = Sig.runInputT Sig.defaultSettings $ Sig.withInterrupt
    $ Sig.handleInterrupt (liftIO (initShutdown chansT lSock))
    $ f

initShutdown :: TVar [(Location, NodeChan)] -> Socket -> IO ()
initShutdown chansT lSock = do
    chansLoc <- readTVarIO chansT
    mapM_ ((`writeChan` (InitLeaveMessage, lSock)) . snd) chansLoc
    return ()


joinCircle nodeGen lSock chansT port quitNow joinLocation = do
    (node, chan) <- newNode nodeGen chansT quitNow Joining
    let jAddr      = joinAddr joinLocation
        jPort      = joinPort joinLocation
        split sock = putStrLn "[Action] Splitting initiated" >> sendMessage sock splitMsg
        splitMsg   = SplitEdgeMessage "127.0.0.1" port (_location node)
        loc        = _location node
    forkIO $ void $ connectAndHandleSafe jAddr jPort (loc, chan) split
    return (node, chan)

newNode :: (ProtocolState -> IO Node ) -> TVar [(Location, NodeChan)]
        -> MVar Bool -> ProtocolState -> IO (Node, NodeChan)
newNode nodeGen chansT quitNow state  = do
    node <- nodeGen state
    chan <- newChan
    atomically $ modifyTVar' chansT ((_location node, chan):)
    putStrLn $ "[State] created new node: " ++ show node
    forkIO $ handleNode node chan chansT quitNow
    return (node, chan)

sendContentMessages :: NodeID -> Socket -> TVar [(Location, NodeChan)] -> IO ()
sendContentMessages serverID sock chansT = do
    chans <- readTVarIO chansT
    (_, chan) <- pick chans
    threadDelay 2000000
    writeChan chan (SendContentMessage serverID, sock)
    threadDelay 2000000
    unless (null chans) $ sendContentMessages serverID sock chansT
    return ()

socketToMessages :: (Location, NodeChan) -> Socket
                 -> Maybe (TVar [(Location, NodeChan)]) -> IO ()
socketToMessages (loc, chan) rSock mChansT = do
    handle handleError convert
  where handleError :: SomeException -> IO ()
        handleError _ = writeChan chan (ShutdownMessage, rSock)
        locChan mLoc cs = do
            chans <- readTVarIO cs
            return $ head $ filter (\(l, _) -> l == mLoc) chans
        convert = do
            bytes <- NLS.getContents rSock
            case mChansT of
                Just chansT -> do
                    let mMLoc = case M.byteStringToMessage bytes of
                                  (Just (HelloCWMessage  _ _ _ l), _) -> Just l
                                  (Just (HelloCCWMessage _ _ _ l), _) -> Just l
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

handleNode :: Node -> Chan (Message, Socket) -> TVar [(Location, NodeChan)] -> MVar Bool-> IO ()
handleNode self chan chansT quitNow
    | isDone self = do
        putStrLn ("[State] Node done! " ++ show (_location self))
        atomically (modifyTVar' chansT (delete (loc, chan)))
        chans <- readTVarIO chansT
        when (null chans) $ putStrLn "[State] All Nodes Done" >> putMVar quitNow True
        return ()
    | otherwise = do
        putStrLn "-------------------------------"
        print self
        putStrLn ". . . . . . . . . . . . . . . ."
        (msg, rSock) <- readChan chan
        answer msg self rSock (loc, chan) >>= recurse
  where recurse node = handleNode node chan chansT quitNow
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
        if isStarved node
           then do
              putStrLn "starved"
              forAllSockets_ node closeSafe
              return $ deletePeer node rSock & protocolState .~ Done
            else return $ deletePeer node rSock
    | _state node == Splitting && Just rSock == cwSocket && isJust (_otherPeer node) = do
        putStrLn $ "[Handling] Shutdown. Splitting -> Close. " ++ show (_location node) ++ " - " ++ show rSock
        closeSafe rSock
        let freeNode = node & protocolState .~ Free
        case _otherPeer node of
            Nothing -> return freeNode
            _       -> return $ freeNode & otherPeer .~ Nothing & cwPeer .~ _otherPeer node
    | otherwise = putStrLn ("[Handling] Shutdown. Closing. " ++ show (_location node) ++ " - " ++ show rSock )>>
                  closeSafe rSock >> return (deletePeer node rSock)
  where cwSocket = fmap _pSocket (_cwPeer node)
        deletePeer :: Node -> Socket -> Node
        deletePeer node sock
            | nSocket node _otherPeer == Just sock = node & otherPeer .~ Nothing
            | nSocket node _cwPeer    == Just sock = node & cwPeer    .~ Nothing
            | nSocket node _ccwPeer   == Just sock = node & ccwPeer   .~ Nothing
            | otherwise                               = node

answer (SplitEdgeMessage rAddr rPort rLoc) node rSock  chan
    | isBusy node              = putStrLn ("[Handling] SplitEdge. Is Busy! " ++ show (_location node) ) >>
                                 closeSafe rSock >> return node
    | isJust (_otherPeer node) = putStrLn ("[Handling] SplitEdge. Has other Peer! " ++ show (_location node))  >>
                                 closeSafe rSock >> return node
    | isNothing (_cwPeer node) = do
        putStrLn $ "[Handling] SplitEdge. No Peers. " ++ show (_location node)
        forkIO $ sendMessage rSock helloCCW
        Just pSock <- connectAndHandleSafe rAddr rPort chan helloCW
        return $ node & ccwPeer .~ Just (Peer pSock rAddr rPort rLoc True) & cwPeer .~ newOtherPeer
    | otherwise = do
        putStrLn $ "[Handling] SplitEdge. Normal Operation. " ++ show (_location node)
        _ <- forkIO $ sendMessage rSock helloCCW
        _ <- forkIO $ sendMessage (fromJust (_cwPeer node)) redirect
        return newNode
  where lLoc         = _location node
        newNode      = node & otherPeer .~ newOtherPeer & protocolState .~ Splitting
        helloCCW     = HelloCCWMessage (_nAddr node) (_nPort node)  lLoc rLoc
        helloCW sock = sendMessage sock $ HelloCWMessage (_nAddr node) (_nPort node) (_location node) rLoc
        redirect     = RedirectMessage rAddr rPort rLoc
        newOtherPeer = Just $ Peer rSock rAddr rPort rLoc True


answer (HelloCCWMessage rAddr rPort srcLoc trgLoc) node rSock _
    | trgLoc /= _location node = putStrLn ("[Handling] HelloCCW. Location mismatch (CCW). "  ++ show (_location node) )>>
                                 closeSafe rSock >> return node
    | otherwise = putStrLn ("[Handling] HelloCCW. All good." ++ show (_location node) ) >>
                  return newNode
  where newNode = node & ccwPeer .~ peer & otherPeer .~ Nothing
        peer = Just $ Peer rSock rAddr rPort srcLoc True

-- HelloCW always denotes the end of the protocol (end of a join), so we
-- can set the new peers and mark the node (ourselfes) as Free.
-- We have now successfully joined the network!
answer (HelloCWMessage rAddr rPort srcLoc trgLoc) node rSock _
    | trgLoc /= _location node = putStrLn "[Handling] HelloCW. Location mismatch." >>
                                 closeSafe rSock >> return node
    | otherwise = putStrLn ("[Handling] HelloCW. All good." ++ show (_location node)) >>
                  return (newNode & protocolState .~ Free)
  where newNode = node & cwPeer .~ peer & otherPeer .~ Nothing
        peer = Just $ Peer rSock rAddr rPort srcLoc True

answer (RedirectMessage rAddr rPort trgLoc) node rSock chan
    | isBusy node = do
        putStrLn ("[Handling] Redirect " ++ (show trgLoc) ++ ". Is Busy. Canceling." ++ show (_location node))
        sendMessage rSock CancelMessage
        return node
    | Just rSock /= fmap _pSocket (_ccwPeer node) = do
        putStrLn "[Handling] Redirect. Invalid peer!. Canceling."
        closeNode node rSock
    | otherwise = do
        putStrLn $ "[Handling] Redirect. All good. " ++ show (_location node)
        putStrLn $ "[Action] greeting new ccw: " ++ (show trgLoc) ++ ". " ++ show (_location node)
        status <- connectAndHandleSafe rAddr rPort chan hello
        case status of
            Just pSock -> handleSuccess pSock
            _          -> cancel >> return node
  where hello sock = sendMessage sock $ HelloCWMessage (_nAddr node) (_nPort node) (_location node) trgLoc
        cancel     = sendMessage rSock CancelMessage >> closeNode node rSock
        handleSuccess sock = do
            putStrLn "[Action] shutting down connection to old ccw peer"
            mapM_ (`sendMessage` ShutdownMessage) $ maybeToList $ _ccwPeer node
            mapM_ closeSafe $ maybeToList $ nSocket node _ccwPeer
            return $ node & otherPeer .~ _ccwPeer node & ccwPeer .~ Just newPeer
          where newPeer = Peer sock rAddr rPort trgLoc True

answer CancelMessage node rSock _ = do
        putStrLn $ "[Handling] Cancel. All good. Shutting down Send. " ++ show rSock ++ show (_location node)
        print node
        mapM_ closeSafe $ maybeToList $ nSocket node _otherPeer
        return $ node & otherPeer .~ Nothing & protocolState .~ Free

answer (MergeEdgeMessage rAddr rPort trgLoc) node rSock chan
    | isBusy node = do
        putStrLn ("[Handling] MergeEdge " ++ (show trgLoc) ++ ". Is Busy. Canceling." ++ show (_location node))
        sendMessage rSock TryLaterMessage
        return node
    | Just rSock /= fmap _pSocket (_cwPeer node) = do
        putStrLn "[Handling] MergeEdge. Invalid peer!. Canceling."
        closeNode node rSock
    | otherwise = do
        putStrLn $ "[Handling] MergeEdge. All good" ++ show (_location node)
        putStrLn $ "[Action] greeting new cw:" ++ (show trgLoc) ++ ". "  ++ show (_location node)
        status <- connectAndHandleSafe rAddr rPort chan hello
        case status of
            Just _ -> handleSuccess rSock
            _      -> cancel >> return node
  where hello sock = sendMessage sock $ HelloCCWMessage (_nAddr node) (_nPort node) (_location node) trgLoc
        cancel     = sendMessage rSock TryLaterMessage
        handleSuccess sock = do
            putStrLn $ "[Action] shutting down connection to old cw peer. " ++ show (_location node)
            closeSafe rSock
            return $ node & cwPeer .~ Just newPeer
          where newPeer = Peer sock rAddr rPort trgLoc True

answer TryLaterMessage node unsafeSock (_, chan) = do
    _ <- forkIO $ do
        threadDelay 1000000
        writeChan chan (InitLeaveMessage, unsafeSock)
    putStrLn $ "[Action] Trying later... " ++ show (_location node)
    return node


answer msg@(ContentMessage srcNodeID srcLoc content) node _ _ = do
   when ((nodeID, loc) /= (srcNodeID, srcLoc)) $ do
       putStrLn $ "[Handling] Forwarding content message. To cw peer: " ++ show (_cwPeer node) ++ " from " ++ show (_location node)
       mapM_ (`sendMessage` newMsg) (maybeToList $ _cwPeer node)
       return ()
   when ((nodeID, loc) == (srcNodeID, srcLoc)) $ do
       -- TODO: REFACTOR!!!
       putStrLn $ "[Handling] Content message Back!" ++ show (_location node)
       putStrLn $ "[Handling] Number of Nodes: " ++ show (length  spl)
       writeFile "p2p.dot" ("digraph p2p {\n" ++ ((C8.unpack content) ++ " -> " ++ circular) ++ ";\n}")
       writeFile "p2pMG.dot" ("digraph p2p {\n" ++ ((C8.unpack ((BS.intercalate (C8.pack " ->") . (fmap (BS.take 14)) . C8.split '>') content)) ++ " -> " ++ circular) ++ ";\n}")
   return node
  where nodeID    = _nodeID node
        loc       = _location node
        spl       = C8.split '>' content
        circular  = (C8.unpack (BS.take 15 content))
        newMsg    = ContentMessage srcNodeID srcLoc newContent
        newContent = content `BS.append` (C8.pack $ " -> N" ++ (hex nodeID) ++ (hex (BS.singleton loc)) )
        hex :: BS.ByteString -> String
        hex = concatMap (printf "%02x") . BS.unpack

answer (SendContentMessage nodeID) node _ _ = do
    putStrLn "[Handling] send content message"
    mapM_ (`sendMessage` msg) $ maybeToList (_cwPeer node)
    return node
  where msg = ContentMessage nodeID loc c
        loc = _location node
        c = C8.pack $ "N" ++ (hex nodeID) ++ (hex (BS.singleton loc))
        hex :: BS.ByteString -> String
        hex = concatMap (printf "%02x") . BS.unpack

answer InitLeaveMessage node unsafeSock (_, chan)
    | isFree node = case node of
        (Node {_cwPeer = Just cwPeer, _ccwPeer = Just ccwPeer}) -> do
            putStrLn $ "[Action] Leaving initiated. " ++ show (_location node)
            sendMessage ccwPeer $ MergeEdgeMessage (_pAddr ccwPeer) (_pPort ccwPeer) (_pLocation ccwPeer)
            return $ node & protocolState .~ Merging
        _ -> do
            putStrLn $ "[Action] Leaving but no neighbours... Quitting. " ++ show (_location node)
            return $ node & protocolState .~ Done
    | otherwise = do
        putStrLn $ "[Action] Leaving but cannot leave right now... Trying later. " ++ show (_location node)
        _ <- forkIO $ do
            threadDelay 1000000
            writeChan chan (InitLeaveMessage, unsafeSock)
        return node

sendMessage :: HasSocket a => a -> Message -> IO ()
sendMessage rSocky msg@(ContentMessage {}) = do
    putStrLn ("[Action] Sending Content Message to Socket " ++ show (getSocket rSocky))
    NBS.sendAll (getSocket rSocky) (M.messageToByteString msg)
sendMessage rSocky msg = go
    where rSock = getSocket rSocky
          go = putStrLn ("[Action] Sending Message: " ++ show msg ++ " to Socket " ++ show rSock) >>
              NBS.sendAll rSock (M.messageToByteString msg)

closeSafe sock = do
    putStrLn $ "[Conn] closing connection (socket)..." ++ show sock
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
