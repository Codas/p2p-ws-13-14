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

import qualified Data.ByteString.Lazy           as LS
import qualified Data.ByteString.Lazy.Char8     as C8
import           Data.List
import           Data.Maybe
import qualified Data.Text                      as Text
import qualified Network.Simple.TCP             as Net
import           Network.Socket                 (ShutdownCmd (ShutdownSend, ShutdownReceive),
                                                 sClose, send, shutdown)
import qualified Network.Socket                 as Sock
import qualified Network.Socket.ByteString      as BLS
import qualified Network.Socket.ByteString.Lazy as NLS
import           Prelude
import           System.IO
import           System.Random                  (randomRIO)
import qualified Text.Read                      as R

import qualified System.Console.Haskeline       as Sig
import           System.Exit                    (exitFailure)

import qualified P2P.Marshalling                as M
import           P2P.Messages
import           P2P.Nodes



-----------------------------------
-- Command line argument parsing --
-----------------------------------
data Opts = Opts
  { opPort    :: String
  , opAddress :: Net.HostPreference
  , opJoins   :: [JoinLocation]
  , opConsole :: Bool
  , opGui     :: Bool}
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
         ( long "console"
        <> short 'c'
        <> help "Enable console logging of connections, msg size etc." )
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

type NodeChan = Chan (Message, Net.Socket)

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
    cMsgsT   <- newTVarIO ([] :: [(NodeID, Location)])

    forkIO $ Sig.runInputT Sig.defaultSettings $ Sig.withInterrupt $ interruptHandler


    let addr          = opAddress options
        port          = opPort options
        joinLocations = opJoins options
    Net.listen addr port $ \(lSock, lAddr) -> do
        putStrLn $ "[Conn] listening on: " ++ show lAddr
        mapM_ (joinCircle nodeGen lSock chansT port cMsgsT) joinLocations
        when (null joinLocations) $ void $ newNode nodeGen lSock chansT cMsgsT
        forever $ Net.acceptFork lSock $ \(rSock, rAddr) -> do
            putStrLn $ "[Conn] accepted new connection: " ++ show rAddr
            chans <- readTVarIO chansT
            chan <- pick chans
            socketToMessages chan rSock cMsgsT

data Signal = Continue | Interrupt

interruptHandler :: Sig.InputT IO ()
interruptHandler = do
    t <- Sig.handleInterrupt (return (Interrupt)) $ do
        Sig.getInputLine ""
        return (Continue)
    case t of
        Interrupt -> shutdownServer
        Continue  -> interruptHandler

shutdownServer :: Sig.InputT IO ()
shutdownServer = do
    Sig.outputStrLn "!SIGINT!"
    return ()

joinCircle nodeGen lSock chansT port cMsgsT joinLocation = do
    (node, chan) <- newNode nodeGen lSock chansT cMsgsT
    let jAddr      = joinAddr joinLocation
        jPort      = joinPort joinLocation
        split sock = putStrLn "[Action] Splitting initiated" >> sendMessage sock splitMsg
        splitMsg   = SplitEdgeMessage "127.0.0.1" port (_location node)
        loc        = _location node
    connectAndHandleSafe jAddr jPort (loc, chan) cMsgsT split
    return (node, chan)

newNode :: IO Node -> Net.Socket -> TVar [(Location, NodeChan)]
        -> TVar [(NodeID, Location)] -> IO (Node, NodeChan)
newNode nodeGen lSock chansT cMsgT = do
    node <- nodeGen
    chan <- newChan
    atomically $ modifyTVar' chansT ((_location node, chan):)
    putStrLn $ "[State] created new node: " ++ show node
    forkIO $ handleNode node chan chansT cMsgT
    return (node, chan)

sendContentMessages :: NodeID -> Net.Socket -> TVar [(Location, NodeChan)] -> IO ()
sendContentMessages serverID lSock chansT = do
    chans <- readTVarIO chansT
    (_, chan) <- pick chans
    writeChan chan (SendContentMessage serverID (Text.pack "hallo leute!"), lSock)
    unless (null chans) $
       sendContentMessages serverID lSock chansT

socketToMessages :: (Location, NodeChan) -> Net.Socket -> TVar [(NodeID, Location)] -> IO ()
socketToMessages (loc, chan) sock cMsgT = do
    handle handleError $ convert
    putStrLn "[Conn State] DONE!"
  where handleError :: SomeException -> IO ()
        handleError e = do
            putStrLn $ "[Exception] " ++ show e
            putStrLn $ "[Exception] Socket: " ++ show sock
            writeChan chan (Shutdown, sock)
        convert = do
            bytes <- NLS.getContents sock
            bytesToMessages chan sock cMsgT bytes

-- Just read every command
bytesToMessages :: NodeChan -> Net.Socket -> TVar [(NodeID, Location)] -> LS.ByteString -> IO ()
bytesToMessages chan rSock cMsgsT bs
    | LS.null bs = putStrLn "[Conn] Disconnected" >> writeChan chan (Shutdown, rSock)
    | otherwise  =
        case M.byteStringToMessage bs of
            (Just msg, rest) -> do
                putStrLn $ "[Message] new message: " ++ show msg
                writeChan chan (msg, rSock)
                bytesToMessages chan rSock cMsgsT rest
            (_, rest) ->
                putStrLn "[Message] got 'something'..." >>
                print (C8.unpack rest) >>
                bytesToMessages chan rSock cMsgsT rest

handleNode :: Node -> Chan (Message, Net.Socket) -> TVar [(Location, NodeChan)]
           -> TVar [(NodeID, Location)]  -> IO ()
handleNode self chan chansT cMsgT
    | isDone self = putStrLn "[State] Node done!" >>
                    atomically (modifyTVar' chansT (delete (loc, chan)))
    | otherwise = do
        (msg, rSock) <- readChan chan
        answer msg self rSock (loc, chan) cMsgT >>= recurse
  where recurse node = handleNode node chan chansT cMsgT
        loc = _location self

-- Some peer just disconnected. Check if it is of intereset for us, conditionally
-- update the node record, than continue
answer :: Message -> Node -> Net.Socket -> (Location, NodeChan)
       -> TVar [(NodeID, Location)] -> IO Node
answer Shutdown node rSock _ _
    | _state node == Merging = do
        putStrLn $ "[Handling] Shutdown. Merging -> Done. " ++ show rSock
        when (isStarved node) $ forAllSockets_ node sClose
        sClose rSock >> return (deletePeer node rSock & protocolState .~ Done)
    | _state node == Splitting = do
        putStrLn $ "[Handling] Shutdown. Splitting -> Close. " ++ show rSock
        return $ node & otherPeer .~ Nothing & cwPeer .~ _otherPeer node & protocolState .~ Free
    | otherwise = putStrLn ("[Handling] Shutdown. Closing. " ++ show rSock )>>
                  sClose rSock >> return (deletePeer node rSock)
  where deletePeer :: Node -> Net.Socket -> Node
        deletePeer node peer
            | nodeSocket node _cwPeer    == Just peer = node & cwPeer    .~ Nothing
            | nodeSocket node _ccwPeer   == Just peer = node & ccwPeer   .~ Nothing
            | nodeSocket node _otherPeer == Just peer = node & otherPeer .~ Nothing
            | otherwise                               = node

answer (SplitEdgeMessage rAddr rPort rLoc) node rSock _ _
    | isBusy node              = putStrLn "[Handling] SplitEdge. Is Busy!" >>
                                 close node rSock
    | isJust (_otherPeer node) = putStrLn "[Handling] SplitEdge. Has other Peer!" >>
                                 close node rSock
    | isNothing (_cwPeer node) = do
        putStrLn "[Handling] SplitEdge. No Peers."
        sendMessage rSock helloCCW
        sendMessage rSock helloCW
        return $ node & cwPeer .~ newPeer & ccwPeer .~ newPeer
    | otherwise = do
        putStrLn "[Handling] SplitEdge. Normal Operation."
        _ <- forkIO $ sendMessage rSock helloCCW
        _ <- forkIO $ sendMessage (_socket (fromJust (_cwPeer node))) redirect
        return $ newNode
  where lLoc         = _location node
        newNode      = node & otherPeer .~ newPeer & protocolState .~ Splitting
        helloCCW     = HelloCCWMessage lLoc rLoc
        helloCW      = HelloCWMessage lLoc rLoc
        redirect     = RedirectMessage rAddr rPort rLoc
        newPeer      = Just $ Peer rSock True rLoc


answer (HelloCCWMessage srcLoc trgLoc) node rSock _ _
    | trgLoc /= _location node = putStrLn "[Handling] HelloCCW. Location mismatch (CCW)." >>
                                 close node rSock
    | otherwise = putStrLn "[Handling] HelloCCW. All good." >>
                  return newNode
  where newNode = node & ccwPeer .~ peer & otherPeer .~ Nothing
        peer = Just $ Peer rSock True srcLoc

-- HelloCW always denotes the end of the protocol (end of a join), so we
-- can set the new peers and mark the node (ourselfes) as Free.
-- We have now successfully joined the network!
answer (HelloCWMessage srcLoc trgLoc) node rSock _ _
    | trgLoc /= _location node = putStrLn "[Handling] HelloCW. Location mismatch." >>
                                 close node rSock
    | otherwise = putStrLn "[Handling] HelloCW. All good." >>
                  return (newNode & protocolState .~ Free)
  where newNode = node & cwPeer .~ peer & otherPeer .~ Nothing
        peer = Just $ Peer rSock True srcLoc

answer (RedirectMessage addr port trgLoc) node rSock chan cMsgT
    | isBusy node                                = putStrLn "[Handling] Redirect. Is Busy. Canceling." >>
                                                   cancel
    | Just rSock /= fmap _socket (_ccwPeer node) = putStrLn "[Handling] Redircet. Invalid peer!. Canceling." >>
                                                   cancel
    | otherwise = do
        putStrLn "[Handling] Redirect. All good"
        putStrLn "greeting new cw"
        status <- connectAndHandleSafe addr port chan cMsgT hello
        case status of
            Just pSock -> handleSuccess pSock
            _          -> cancel >> return node
  where hello sock  = sendMessage sock $ HelloCWMessage (_location node) trgLoc
        cancel      = sendMessage rSock CancelMessage >> close node rSock
        handleSuccess pSock = do
            putStrLn "shutting down connection do ccw peer"
            mapM_ sClose $ maybeToList $ nodeSocket node _ccwPeer
            return $ node & otherPeer .~ _ccwPeer node & ccwPeer .~ Just newPeer
          where newPeer = Peer pSock True trgLoc

answer CancelMessage node rSock _ _
    | nodeSocket node _cwPeer /= Just rSock = putStrLn "[Handling] Cancel. Doing nothing." >>
                                              return node
    | otherwise = do
        putStrLn $ "[Handling] Cancel. All good. Shutting down Send. " ++ show rSock
        print node
        mapM_ (`shutdown` ShutdownSend) $ maybeToList $ nodeSocket node _otherPeer
        return $ node & otherPeer .~ Nothing

answer (MergeEdgeMessage addr port trgLoc) node rSock _ _ = undefined
answer TryLaterMessage node rSock _ _ = undefined
answer msg@(ContentMessage srcNodeID srcLoc content) node _ _ _
    | or $ fmap ((== Just srcLoc) . nodeLocation node) [_ccwPeer, _cwPeer] = do
        putStrLn "[Handling] Content. All Good."
        print content -- debugging only
        when ((nodeID, loc) /= (srcNodeID, srcLoc)) $ do
            when (cwLoc  == Just srcLoc) $ mapM_ (`sendMessage` newMsg) ccwSocket
            when (ccwLoc == Just srcLoc) $ mapM_ (`sendMessage` newMsg) cwSocket
        return node
    | otherwise = putStrLn "[Handling] Content. Invalid Peer." >>
                  return node
  where cwLoc     = nodeLocation node _cwPeer
        ccwLoc    = nodeLocation node _ccwPeer
        cwSocket  = maybeToList $ nodeSocket node _cwPeer
        ccwSocket = maybeToList $ nodeSocket node _ccwPeer
        newMsg    = msg & _srcLoc .~ _location node
        nodeID    = _nodeID node
        loc       = _location node

answer (SendContentMessage nodeID content) node _ _ _ = do
    mapM_ (`sendMessage` ContentMessage nodeID loc content) $ maybeToList sock
    return node
  where sock = fmap _socket $ _ccwPeer node
        loc = _location node

sendMessage :: Net.Socket -> Message -> IO ()
sendMessage rSock msg = (putStrLn $ "[Action] Sending Message: " ++ show msg) >>
                        (BLS.sendAll rSock $ M.messageToByteString msg)

close :: Node -> Net.Socket -> IO Node
close node rSock = (putStrLn $ "[Action] Close! " ++ show rSock) >>
                   sClose rSock >> return node

------------------------------------------
-- Async safe connections with timeouts --
------------------------------------------

connectAndHandleSafe :: String -> String -> (Location, NodeChan) -> TVar [(NodeID, Location)]
                     -> (Net.Socket -> IO ()) -> IO (Maybe Net.Socket)
connectAndHandleSafe addr port chan cMsgT action = do
    mvar <- newEmptyMVar
    let handleError :: SomeException -> IO ()
        handleError _ = putMVar mvar Nothing
    tid <- forkIO $ handle handleError $ connect mvar
    success <- timeout 2000000 $ takeMVar mvar
    case success of
         Just (Just rSock) -> (action rSock) >> return (Just rSock)
         _               -> killThread tid >> putStrLn failedMsg >> return Nothing
  where failedMsg = "failed to connect to: " ++ addr ++ ":" ++ port
        connect mvar = Net.connect addr port $ \(sock, rAddr) -> do
            putStrLn ("connected to: " ++ show rAddr)
            putMVar mvar (Just sock)
            socketToMessages chan sock cMsgT

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
