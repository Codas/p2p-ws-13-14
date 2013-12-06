module Server where

import           Control.Applicative
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Chan        (Chan, newChan, readChan,
                                                 writeChan)
import           Control.Exception              (SomeException, handle)
import           Control.Lens
import           Control.Lens.Setter
import           Control.Monad
import           Control.Monad.Trans            (liftIO)
import           Options.Applicative            hiding ((&))
import           System.Timeout

import qualified Data.ByteString.Lazy           as LS
import           Data.Maybe
import qualified Network.Simple.TCP             as Net
import           Network.Socket                 (ShutdownCmd (ShutdownSend),
                                                 sClose, shutdown)
import qualified Network.Socket.ByteString      as BLS
import qualified Network.Socket.ByteString.Lazy as NLS
import           Prelude
import qualified Text.Read                      as R

import qualified P2P.Marshalling                as M
import           P2P.Messages
import           P2P.Nodes


import           Control.Concurrent             (killThread, threadDelay)
import           Control.Concurrent.MVar        (newEmptyMVar, putMVar,
                                                 takeMVar)

-----------------------------------
-- Command line argument parsing --
-----------------------------------
data Opts = Opts
  { opPort    :: String
  , opAddress :: Net.HostPreference
  , opRelay   :: Maybe (Net.HostName, Int)
  , opConsole :: Bool
  , opGui     :: Bool
  , opGuiPort :: Int}
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
     <*> nullOption
         ( long "relay"
        <> short 'r'
        <> value Nothing
        <> metavar "RELAY"
        <> reader relayReader
        <> showDefault
        <> help "Relay to connect to.")
     <*> switch
         ( long "console"
        <> short 'c'
        <> help "Enable console logging of connections, msg size etc." )
     <*> switch
         ( long "gui"
        <> short 'g'
        <> help "Enable web based graphical user interface. Binds do port 10000." )
     <*> nullOption
         ( long "gport"
        <> value 10000
        <> metavar "GUIPORT"
        <> reader auto
        <> showDefault
        <> help "GUI Port.")

-- custom reader to parse the host preferencec
hostReader :: Monad m => String -> m Net.HostPreference
hostReader s = return $ Net.Host s

relayReader :: Monad m => String -> m ( Maybe ( Net.HostName, Int ))
relayReader s = case R.readMaybe (tail i) :: (Maybe Int) of
    Just int -> return $ Just (string, int)
    _        -> return Nothing
  where (string, i) = break (== ':') s

-- build the command line options, including helper text and parser
opts :: ParserInfo Opts
opts = info (serverOpts <**> helper)
  ( fullDesc
 <> progDesc "Start a TCP server and listen to incomming connections"
 <> header "Server - one Server to bring them all and to the socket bind them" )


--------------------------------------------------------------
-- Main server logic. Listen, accept, send events and pring --
--------------------------------------------------------------
main :: IO ()
main = Net.withSocketsDo $ do
    options <- execParser opts
    serverID <- newServerID
    newNode <- newNodeGenerator serverID
    let address = opAddress options
        port    = opPort options
    Net.listen address port $ \(lSock, lAddr) -> do
        chan <- newChan
        node <- newNode
        forkIO $ handleNode node chan lSock
        forever $ Net.acceptFork lSock $ \(rSock, rAddr) ->  do
            socketToMessages chan rSock

socketToMessages :: Chan (Message, Net.Socket) -> Net.Socket -> IO ()
socketToMessages chan sock = do
    bytes <- NLS.getContents sock
    bytesToMessages chan sock bytes

-- Just read every command
bytesToMessages :: Chan (Message, Net.Socket) -> Net.Socket -> LS.ByteString -> IO ()
bytesToMessages chan rSock bs
    | LS.null bs = writeChan chan (Shutdown, rSock)
    | otherwise  =
        case M.byteStringToMessage bs of
            (Just msg, rest) -> do
                writeChan chan (msg, rSock)
                bytesToMessages chan rSock rest
            (_, rest) -> bytesToMessages chan rSock rest

handleNode :: Node -> Chan (Message, Net.Socket) -> Net.Socket -> IO ()
handleNode self chan lSock
    | isDone self = return ()
    | otherwise = do
        (msg, rSock) <- readChan chan
        answer msg self rSock chan >>= recurse
  where recurse s = handleNode s chan lSock

-- Some peer just disconnected. Check if it is of intereset for us, conditionally
-- update the node record, than continue
answer :: Message -> Node -> Net.Socket -> Chan (Message, Net.Socket) -> IO Node
answer Shutdown node rSock _
    | _state node == Merging = do
        when (isStarved node) $ forAllSockets_ node sClose
        sClose rSock >> return (deletePeer node rSock & protocolState .~ Done)
    | _state node == Splitting = do
        sClose rSock
        return (deletePeer node rSock & cwPeer .~ _otherPeer node & protocolState .~ Free)
    | otherwise = sClose rSock >> return (deletePeer node rSock)
  where deletePeer :: Node -> Net.Socket -> Node
        deletePeer node peer
            | nodeSocket node _cwPeer    == Just peer = node & cwPeer    .~ Nothing
            | nodeSocket node _ccwPeer   == Just peer = node & ccwPeer   .~ Nothing
            | nodeSocket node _otherPeer == Just peer = node & otherPeer .~ Nothing
            | otherwise                               = node

answer (SplitEdgeMessage rAddr rPort rLoc) node rSock _
    | isBusy node              = close node rSock
    | isJust (_otherPeer node) = close node rSock
    | isNothing (_cwPeer node) = close node rSock
    | otherwise = do
        _ <- forkIO $ sendMessage rSock hello
        _ <- forkIO $ sendMessage (_socket (fromJust (_cwPeer node))) redirect
        return $ node & otherPeer .~ newPeer & protocolState .~ Splitting
  where lLoc     = _location node
        newPeer  = Just $ Peer rSock True rLoc
        hello    = HelloCCWMessage lLoc rLoc
        redirect = RedirectMessage rAddr rPort lLoc


answer (HelloCCWMessage srcLoc trgLoc) node rSock _
    | trgLoc /= _location node                         = close node rSock
    | Just (Peer rSock True srcLoc) /= _otherPeer node = close node rSock
    | otherwise = return $ node & ccwPeer .~ (_otherPeer node) & otherPeer .~ Nothing

-- HelloCW always denotes the end of the protocol (end of a join), so we
-- can set the new peers and mark the node (ourselfes) as Free.
-- We have now successfully joined the network!
answer (HelloCWMessage srcLoc trgLoc) node rSock _
    | trgLoc /= _location node                         = close node rSock
    | Just (Peer rSock True srcLoc) /= _otherPeer node = close node rSock
    | otherwise = return $ newNode & protocolState .~ Free
  where newNode = node & cwPeer .~ (_otherPeer node) & otherPeer .~ Nothing

answer (RedirectMessage addr port trgLoc) node rSock chan
    | isBusy node                                = cancel
    | trgLoc /= _location node                   = cancel
    | Just rSock /= fmap _socket (_ccwPeer node) = cancel
    | otherwise = do
        status <- connectAndHandleSafe addr port chan hello
        case status of
            Just pSock -> handleSuccess pSock
            _          -> cancel >> return node
  where hello       = sendMessage rSock $ HelloCWMessage (_location node) trgLoc
        cancel      = sendMessage rSock CancelMessage >> close node rSock
        handleSuccess pSock = do
            mapM_ (`shutdown` ShutdownSend) $ maybeToList $ nodeSocket node _ccwPeer
            return $ node & otherPeer .~ _ccwPeer node & ccwPeer .~ Just newPeer
          where newPeer = Peer pSock True trgLoc

answer CancelMessage node rSock _
    | nodeSocket node _otherPeer /= Just rSock = return node
    | otherwise = do
        mapM_ (`shutdown` ShutdownSend) $ maybeToList $ nodeSocket node _otherPeer
        return $ node & otherPeer .~ Nothing

answer (MergeEdgeMessage addr port trgLoc) node rSock _ = undefined
answer (ContentMessage _ _ srcLoc content) node rSock _ = undefined
answer TryLaterMessage node rSock _ = undefined

sendMessage :: Net.Socket -> Message -> IO ()
sendMessage rSock msg = BLS.sendAll rSock $ M.messageToByteString msg

close :: Node -> Net.Socket -> IO Node
close node rSock = sClose rSock >> return node

------------------------------------------
-- Async safe connections with timeouts --
------------------------------------------

connectAndHandleSafe :: String -> String -> Chan (Message, Net.Socket) -> IO () -> IO (Maybe Net.Socket)
connectAndHandleSafe addr port chan action = do
    mvar <- newEmptyMVar
    let handleError :: SomeException -> IO ()
        handleError _ = putMVar mvar Nothing
    tid <- forkIO $ handle handleError $ connect mvar
    success <- timeout 2000000 $ takeMVar mvar
    case success of
         Just (Just rSock) -> action >> return (Just rSock)
         _               -> killThread tid >> return Nothing
  where connect mvar = Net.connect addr port $ \(sock, _) -> do
            putMVar mvar (Just sock)
            socketToMessages chan sock

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
