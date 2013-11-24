module Server where

import           Control.Applicative
import           Control.Concurrent   (forkIO)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans  (liftIO)
import           Options.Applicative
import           Reactive.Threepenny
import           System.IO

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.List            as L
import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.IO         as Text

import           P2P.Commands
import qualified P2P.Events           as Evt
import qualified P2P.GUI              as GUI
import qualified P2P.Messages         as M
import qualified P2P.Networking       as Net
import qualified P2P.Protocol         as P

import           Debug.Trace

-----------------------------------
-- Command line argument parsing --
-----------------------------------
data Opts = Opts
  { opPort    :: String
  , opAddress :: Net.HostPreference
  , opRelay   :: Maybe String
  , opConsole :: Bool
  , opGui     :: Bool }
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
        <> reader auto
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

-- custom reader to parse the host preferencec
hostReader :: Monad m => String -> m Net.HostPreference
hostReader s = return $ Net.Host s

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
    (netEvent, pEvt) <- Evt.getEventBus
    topicB  <- Evt.getTopicBehavior  netEvent
    clientB <- Evt.getClientBehavior netEvent

    initUIs netEvent clientB options

    startServer options netEvent pEvt topicB clientB
    -- _ <- forkIO $ connectToRelay options netEvent pEvt

    return ()

startServer :: Opts -> Evt.NetEventGetter -> (Evt.NetEvent -> IO b)
            -> Behavior (Set Evt.TopicClients) -> Behavior (Set Evt.ClientCon) -> IO r
startServer options netEvent pEvt topicB clientB = listen $ \(lSock, lAddr) -> do
    let server     = Evt.Client lAddr Nothing 0
        serverInfo = Evt.MessageInfo 0 Set.empty
    _ <- pEvt $ Evt.NetEvent Evt.Ready server serverInfo
    forever . Net.acceptFork lSock $ \(rHandle, rAddr) -> do
        let client = Evt.Client rAddr (Just rHandle) 0
        handleClient rHandle client topicB clientB pEvt
  where listen = Net.listen address port
        address = opAddress options
        port    = opPort options


-- Read content from socket handle, display it and notify system of events
handleClient :: Handle -> Evt.Client -> Behavior (Set Evt.TopicClients)
             -> Behavior (Set Evt.ClientCon) -> (Evt.NetEvent -> IO b) -> IO ()
handleClient handle client topicB clientB pushEvent = do
    let clientInfo = Evt.MessageInfo 0 Set.empty
    _ <- pushEvent $ Evt.NetEvent Evt.Connected client clientInfo
    clientStream <- LS.hGetContents handle
    let eventTuple = (topicB, clientB, pushEvent, client)
    handleProtocol eventTuple clientStream
    _ <- pushEvent $ Evt.NetEvent Evt.Disconnected client clientInfo
    return ()

connectToRelay :: Opts -> Evt.NetEventGetter -> (Evt.NetEvent -> IO b) -> IO ()
connectToRelay options netEvent pEvt = case opRelay options of
    Nothing    -> return ()
    Just rAddr -> undefined

-----------------------------------
-- Protocol and client handling. --
-----------------------------------

type EventTuple b = (Behavior (Set Evt.TopicClients), Behavior (Set Evt.ClientCon)
                    , Evt.NetEvent -> IO b, Evt.Client)

handleProtocol :: EventTuple b -> LS.ByteString -> IO ()
handleProtocol evtTuple bs
    | LS.null bs   = return ()
    | otherwise = do
      let (nMsg, rest) = M.byteStringToMessage bs
      case M.command nMsg of
          Join      -> handleJoin      evtTuple nMsg
          Part      -> handlePart      evtTuple nMsg
          Message   -> handleMessage   evtTuple nMsg
          Broadcast -> handleBroadcast evtTuple nMsg
          _         -> return ()
      handleProtocol evtTuple rest

-- client handling
handleJoin :: EventTuple b -> M.NetMessage -> IO ()
handleJoin (topicB, _, pEvt, client) nMsg = case M.topics nMsg of
    Nothing -> return ()
    Just ts -> do
        currentTs <- currentValue topicB
        let newTopics = ts Set.\\ Set.map Evt._topic currentTs
            i         = Evt.MessageInfo 0 ts
        _ <- pEvt $ Evt.NetEvent Evt.FirstJoin client (Evt.MessageInfo 0 newTopics)
        _ <- pEvt $ Evt.NetEvent Evt.Join client i
        return ()

handlePart :: EventTuple b -> M.NetMessage -> IO ()
handlePart (topicB, _, pEvt, client) nMsg = case M.topics nMsg of
    Nothing -> return ()
    Just ts -> do
        currentTs <- currentValue topicB
        let legTopics = Set.map Evt._topic $ Set.filter (\tc -> Evt._clients tc == cSet) currentTs
            i         = Evt.MessageInfo 0 ts
            cSet = Set.singleton client
        _ <- pEvt $ Evt.NetEvent Evt.LastPart client (Evt.MessageInfo 0 legTopics)
        _ <- pEvt $ Evt.NetEvent Evt.Part client i
        return ()

handleMessage :: EventTuple b -> M.NetMessage -> IO ()
handleMessage (topicB, _, pEvt, client) nMsg = case nMsg of
    (M.NetMessage _ (Just ts) (Just msg)) -> do
        currentTs <- currentValue topicB
        let i       = Evt.MessageInfo (Text.length msg) ts
            binMsg  = M.messageToByteString nMsg
            subTs   = Set.filter (\tc -> Set.member (Evt._topic tc) ts) currentTs
        _ <- pEvt $ Evt.NetEvent Evt.Message client i

        forClientHandles subTs client $ \h -> BS.hPut h binMsg >> hFlush h

        -- TODO: Debugging only, remove when done...
        -- outputs current message to the console
        Text.putStrLn msg
        return ()
    _ -> return ()

handleBroadcast :: EventTuple b -> M.NetMessage -> IO ()
handleBroadcast (_, clientB, pEvt, client) nMsg = case M.message nMsg of
    Nothing  -> return ()
    Just msg -> do
        currentCs <- currentValue clientB
        let i          = Evt.MessageInfo (Text.length msg) Set.empty
            binMsg     = M.messageToByteString nMsg
            cs = mapMaybe (Evt.clientHandle . Evt._client ) $ Set.toList currentCs
        _ <- pEvt $ Evt.NetEvent Evt.Broadcast client i
        forM_ cs $ \h -> BS.hPut h binMsg >> hFlush h

        -- TODO: Debugging only, remove when done...
        -- outputs current message to the console
        Text.putStrLn msg
        return ()

clientsToHandles :: Set Evt.Client -> Evt.Client -> [Handle]
clientsToHandles cs c = mapMaybe Evt.clientHandle noCBClient
  where noCBClient = Set.toList $ Set.filter (/= c) cs

-- concurrently execute function for all client handles in the topicClients set.
-- Exclude single client from the set.
forClientHandles :: Set Evt.TopicClients -> Evt.Client -> (Handle -> IO ()) -> IO ()
forClientHandles tcs c fn = (forM_ handles $ forkIO . fn) >> print handles
  where cs = Set.foldr (Set.union . Evt._clients) Set.empty tcs
        handles = clientsToHandles cs c

--------------------------
-- Events and behaviors --
--------------------------

-------------------------------
-- General UI initialization --
-------------------------------
initUIs :: Evt.NetEventGetter -> Behavior (Set Evt.ClientCon) -> Opts -> IO ()
initUIs netEvent clientB options = do
    when ( opGui options ) $ GUI.init netEvent clientB -- init web GUI
    when (opConsole options) $ initConsoleUI netEvent  -- init console gui

----------------
-- Console UI --
----------------
initConsoleUI :: Evt.NetEventGetter -> IO ()
initConsoleUI netEvent = do
  evtB <- stepper undefined (netEvent Evt.AnyEvent)
  onChange evtB print
