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

import           P2P.Commands
import qualified P2P.Events           as Evt
import qualified P2P.GUI              as GUI
import qualified P2P.Messages         as M
import qualified P2P.Networking       as Net
import qualified P2P.Protocol         as P

-----------------------------------
-- Command line argument parsing --
-----------------------------------
data Opts = Opts
  { opPort    :: String
  , opAddress :: Net.HostPreference
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
        <> metavar "HOST"
        <> reader hostReader
        <> showDefaultWith (const "Any address")
        <> help "Address to accept incoming connections from. E.g. localhost.")
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

    (netEvent, pushEvent) <- Evt.getEventBus
    initUIs netEvent options

  -- listen for incomig connections.
    let address = opAddress options
        port    = opPort options
    -- TODO: server api verwenden?
    Net.listen address port $ \(listenSocket, listenAddr) -> do
        let server = Evt.Client listenAddr Nothing
            serverInfo   = Evt.MessageInfo 0 Set.empty
        pushEvent $ Evt.NetEvent Evt.Ready server serverInfo
        topicB <- getClientBehavior netEvent
        forever . Net.acceptFork listenSocket $ \(connHandle, remoteAddr) -> do
            let client = Evt.Client remoteAddr (Just connHandle)
            handleClient connHandle client topicB pushEvent

-- Read content from socket handle, display it and notify system of events
handleClient :: Handle -> Evt.Client -> Behavior (Set TopicClients) -> (Evt.NetEvent -> IO b) -> IO ()
handleClient handle client topicB pushEvent = do
    let clientInfo = Evt.MessageInfo 0 Set.empty
    _ <- pushEvent $ Evt.NetEvent Evt.Connected client clientInfo
    clientStream <- LS.hGetContents handle
    let eventTuple = (topicB, pushEvent, client)
    handleProtocol eventTuple clientStream
    _ <- pushEvent $ Evt.NetEvent Evt.Disconnected client clientInfo
    return ()

-----------------------------------
-- Protocol and client handling. --
-----------------------------------

type RawMessage = (LS.ByteString, Flags)
type EventTuple b = (Behavior (Set TopicClients), Evt.NetEvent -> IO b, Evt.Client)

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
handleJoin (topicB, pEvt, client) nMsg = case M.topics nMsg of
    Nothing -> return ()
    Just ts -> do
        currentTs <- currentValue topicB
        let newTopics = ts Set.\\ Set.map _topic currentTs
            i         = Evt.MessageInfo 0 ts
        _ <- pEvt $ Evt.NetEvent Evt.FirstJoin client (Evt.MessageInfo 0 newTopics)
        _ <- pEvt $ Evt.NetEvent Evt.Join client i
        return ()


handlePart :: EventTuple b -> M.NetMessage -> IO ()
handlePart (topicB, pEvt, client) nMsg = case M.topics nMsg of
    Nothing -> return ()
    Just ts -> do
        currentTs <- currentValue topicB
        let legTopics = Set.map _topic $ Set.filter (\tc -> _clients tc == cSet) currentTs
            i         = Evt.MessageInfo 0 ts
            cSet = Set.singleton client
        _ <- pEvt $ Evt.NetEvent Evt.LastPart client (Evt.MessageInfo 0 legTopics)
        _ <- pEvt $ Evt.NetEvent Evt.Join client i
        return ()

handleMessage :: EventTuple b -> M.NetMessage -> IO ()
handleMessage (topicB, pEvt, client) nMsg = case nMsg of
    (M.NetMessage _ (Just ts) (Just msg)) -> do
        currentTs <- currentValue topicB
        let i       = Evt.MessageInfo (Text.length msg) ts
            binMsg  = M.messageToByteString nMsg
            subTs   = Set.filter (\tc -> Set.member (_topic tc) ts) currentTs
        _ <- pEvt $ Evt.NetEvent Evt.Message client i
        forClientHandless subTs client $ \h -> BS.hPut h binMsg

        -- TODO: Debugging only, remove when done...
        -- outputs current message to the console
        BS.putStr $ TE.encodeUtf8 msg
        return ()
    _ -> return ()

handleBroadcast :: EventTuple b -> M.NetMessage -> IO ()
handleBroadcast (topicB, pEvt, client) nMsg = case M.message nMsg of
    Nothing  -> return ()
    Just msg -> do
        currentTs <- currentValue topicB
        let i          = Evt.MessageInfo (Text.length msg) Set.empty
            binMsg     = M.messageToByteString nMsg
        _ <- pEvt $ Evt.NetEvent Evt.Broadcast client i
        forClientHandless currentTs client $ \h -> BS.hPut h binMsg

        -- TODO: Debugging only, remove when done...
        -- outputs current message to the console
        BS.putStr $ TE.encodeUtf8 msg
        return ()

clientsToHandles :: Set Evt.Client -> Evt.Client -> [Handle]
clientsToHandles cs c = mapMaybe Evt.clientHandle noCBClient
  where noCBClient = Set.toList $ Set.filter (/= c) cs

-- concurrently execute function for all client handles in the topicClients set.
-- Exclude single client from the set.
forClientHandless :: Set TopicClients -> Evt.Client -> (Handle -> IO ()) -> IO ()
forClientHandless tcs c fn = forM_ handles $ forkIO . fn
  where cs = Set.foldr (Set.union . _clients) Set.empty tcs
        handles = clientsToHandles cs c

--------------------------
-- Events and behaviors --
--------------------------

data ClientCon = ClientCon
                  { _client :: Evt.Client
                  , _topics :: Set Topic}
                  deriving (Show)

instance Eq ClientCon where
    (ClientCon c _)  == (ClientCon c1 _) = c == c1

instance Ord ClientCon where
    compare (ClientCon c _) (ClientCon c1 _) = compare c c1

topics :: Lens' ClientCon (Set Topic)
topics = lens _topics (\clientCon t -> clientCon { _topics = t })

data TopicClients = TopicClients
                  { _topic   :: Topic
                  , _clients :: Set Evt.Client}
                  deriving (Show)

instance Eq TopicClients where
    (TopicClients t _)  == (TopicClients t1 _) = t == t1

instance Ord TopicClients where
    compare (TopicClients t _) (TopicClients t1 _) = compare t t1

clients :: Lens' TopicClients (Set Evt.Client)
clients = lens _clients (\topicClient t -> topicClient { _clients = t })

getClientBehavior :: Evt.NetEventGetter -> IO (Behavior (Set TopicClients))
getClientBehavior evt = accumB Set.empty ( accTopicClients <$> evt Evt.AnyEvent)

accClientCon :: Evt.NetEvent -> Set ClientCon -> Set ClientCon
accClientCon (Evt.NetEvent eType c i) cns =
  case eType of
    Evt.Disconnected -> Set.delete client cns
    Evt.Connected    -> Set.insert client cns
    Evt.Join         -> Set.map (modTopics $ Set.union ets) cns
    Evt.Part         -> Set.map (modTopics (Set.\\ ets)) cns
    _                -> cns
  where ets     = Evt.topics i
        client = ClientCon c ets
        modTopics :: (Set Topic -> Set Topic) -> ClientCon -> ClientCon
        modTopics f cn
            | c == _client cn = over topics f cn
            | otherwise = cn

accTopicClients :: Evt.NetEvent -> Set TopicClients -> Set TopicClients
accTopicClients (Evt.NetEvent eType c i) tcs =
  case eType of
    Evt.Join         -> Set.map (modClients $ Set.insert c) addedTopics
    Evt.Part         -> cleanup $ Set.map (modClients (Set.delete c)) tcs
    _                -> tcs
  where ets         = Evt.topics i
        etc         = Set.map (`TopicClients` Set.empty) ets
        addedTopics = Set.union etc tcs
        cleanup     = Set.filter (\(TopicClients _ cs)  -> not $ Set.null cs)
        modClients :: (Set Evt.Client -> Set Evt.Client) -> TopicClients -> TopicClients
        modClients f tc
            | Set.member (_topic tc) ets = over clients f tc
            | otherwise = tc

accTopics :: Evt.NetEvent -> Set Topic -> Set Topic
accTopics (Evt.NetEvent eType c i) ts =
  case eType of
    Evt.Join         -> Set.union ts ets
    Evt.Part         -> ts Set.\\ ets
    _                -> ts
  where ets = Evt.topics i

-------------------------------
-- General UI initialization --
-------------------------------
initUIs :: Evt.NetEventGetter -> Opts -> IO ()
initUIs netEvent options = do
    when ( opGui options ) $ GUI.init netEvent        -- init web GUI
    when (opConsole options) $ initConsoleUI netEvent -- init console gui

----------------
-- Console UI --
----------------
initConsoleUI :: Evt.NetEventGetter -> IO ()
initConsoleUI netEvent = do
  evtB <- stepper undefined (netEvent Evt.AnyEvent)
  onChange evtB print
