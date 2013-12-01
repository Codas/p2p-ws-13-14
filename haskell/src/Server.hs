module Server where

import           Control.Applicative
import           Control.Concurrent   (forkIO)
import qualified Control.Lens         as L
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
import           Data.Word
import qualified Text.Read            as R

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
    (netEvent, pEvt) <- Evt.getEventBus
    topicB  <- Evt.getTopicBehavior  netEvent
    clientB <- Evt.getClientBehavior netEvent
    joins <- stepper undefined (netEvent Evt.AnyEvent)

    onChange clientB print

    initUIs netEvent clientB options

    forkIO $ connectToRelay options topicB clientB pEvt
    forkIO $ handleRelays clientB joins
    startServer options netEvent pEvt topicB clientB

    return ()

startServer :: Opts -> Evt.NetEventGetter -> (Evt.NetEvent -> IO b)
            -> Behavior (Set Evt.TopicClients) -> Behavior (Set Evt.ClientCon) -> IO r
startServer options netEvent pEvt topicB clientB = listen $ \(lSock, lAddr) -> do
    let server     = Evt.Client lAddr Nothing True 0
        serverInfo = Evt.MessageInfo 0 Set.empty
    _ <- pEvt $ Evt.NetEvent Evt.Ready server serverInfo
    forever . Net.acceptFork lSock $ \(rHandle, rAddr) -> do
        let client = Evt.Client rAddr (Just rHandle) False 0
        handleClient rHandle client topicB clientB pEvt
  where listen = Net.listen address port
        address = opAddress options
        port    = opPort options

-- Read content from socket handle, display it and notify system of events
handleClient :: Handle -> Evt.Client -> Behavior (Set Evt.TopicClients)
             -> Behavior (Set Evt.ClientCon) -> (Evt.NetEvent -> IO b) -> IO ()
handleClient handle client topicB clientB pushEvent = do
    let clientInfo = Evt.MessageInfo 0 Set.empty
    clientStream <- LS.hGetContents handle
    let cor        = if isRelay then relay else client
        eventTuple = (topicB, clientB, pushEvent, cor)
        streamHead = LS.take 1 clientStream
        relayBS    = LS.singleton (248::Word8) -- TODO: Remove magic number
        relay      = L.set Evt.isRelayL True client
        isRelay    = streamHead == relayBS

    when isRelay $ do
        currentTs <- currentValue topicB
        let binTopics  = M.messageToByteString (M.NetMessage Join (Just topics) Nothing)
            topics     = Set.map Evt._topic currentTs
        unless (Set.null topics) $ BS.hPut handle binTopics >> hFlush handle

    _ <- pushEvent $ Evt.NetEvent Evt.Connected cor clientInfo
    handleProtocol eventTuple clientStream
    _ <- pushEvent $ Evt.NetEvent Evt.Disconnected client clientInfo
    return ()

connectToRelay :: Opts -> Behavior (Set Evt.TopicClients)
               -> Behavior (Set Evt.ClientCon) -> (Evt.NetEvent -> IO b) -> IO ()
connectToRelay options topicB clientB pEvt = case opRelay options of
    Nothing    -> return ()
    Just (host, port) ->
        Net.connectTo host port $ \(handle, rAddr) -> do
            currentTs <- currentValue topicB
            let relay      = Evt.Client rAddr (Just handle) True 0
                relayInfo  = Evt.MessageInfo 0 Set.empty
                eventTuple = (topicB, clientB, pEvt, relay)
                topics     = Set.map Evt._topic currentTs
                binRelay   = M.messageToByteString (M.NetMessage Relay Nothing Nothing)
                binTopics  = M.messageToByteString (M.NetMessage Join (Just topics) Nothing)

            BS.hPut handle binRelay
            unless (Set.null topics) $ BS.hPut handle binTopics
            hFlush handle

            pEvt $ Evt.NetEvent Evt.Connected relay relayInfo
            clientStream <- LS.hGetContents handle
            handleProtocol eventTuple clientStream
            pEvt $ Evt.NetEvent Evt.Disconnected relay relayInfo
            return ()

handleRelays :: Behavior (Set Evt.ClientCon) -> Behavior Evt.NetEvent -> IO ()
handleRelays clientB joins =
    onChange joins $ \(Evt.NetEvent eType client (Evt.MessageInfo _ ts)) ->
        case eType of
            Evt.FirstJoin -> do
                currentRs <- currentValue clientB
                let nMsg   = M.NetMessage Join (Just ts) Nothing
                    subRs  = targetedRelays client currentRs
                writeToAllR subRs nMsg
                return ()
            Evt.LastPart -> do
                currentRs <- currentValue clientB
                let nMsg   = M.NetMessage Part (Just ts) Nothing
                    subRs  = targetedRelays client currentRs
                writeToAllR subRs nMsg
                return ()
            _ -> return ()
    -- handleRelayEvents netEvent handle client pEvt

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
            AskTopics -> handleTopicReq  evtTuple nMsg
            _         -> return ()
        handleProtocol evtTuple rest

-- client handling
handleJoin :: EventTuple b -> M.NetMessage -> IO ()
handleJoin (_, clientB, pEvt, client) nMsg = case M.topics nMsg of
    Nothing -> return ()
    Just ts -> do
        currentCs <- currentValue clientB
        let newTopics    = ts Set.\\ Set.foldr Set.union Set.empty topicsSet
            topicsSet    = Set.map Evt._topics clientTopics
            clientTopics = Set.filter (\cn -> not (Evt.isRelay (Evt._client cn))) currentCs
            i            = Evt.MessageInfo 0 ts
        -- unless (Set.null newTopics) $ do
        pEvt $ Evt.NetEvent Evt.FirstJoin client (Evt.MessageInfo 0 newTopics)
            -- return ()
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
            modClients f tc
                | Set.member (Evt._topic tc) ts = L.over Evt.clients f tc
                | otherwise = tc
            modTopics = Set.map (modClients (Set.delete client)) currentTs
            modTopics' = Set.map (modClients (Set.filter (not . Evt.isRelay))) currentTs
        when (Set.null (Set.filter (\tc -> not (Set.null (Evt._clients tc))) modTopics')) $ do
            _ <- pEvt $ Evt.NetEvent Evt.LastPart client (Evt.MessageInfo 0 legTopics)
            return()
        putStrLn "---------------"
        print legTopics
        putStrLn "---------------"
        _ <- pEvt $ Evt.NetEvent Evt.Part client i
        return ()

handleMessage :: EventTuple b -> M.NetMessage -> IO ()
handleMessage (_, clientB, pEvt, client) nMsg = case nMsg of
    (M.NetMessage _ (Just ts) (Just msg)) -> do
        currentCs <- currentValue clientB
        let i            = Evt.MessageInfo (Text.length msg) ts
            subCs        = targetedClients ts client currentCs
        _ <- pEvt $ Evt.NetEvent Evt.Message client i
        writeToAllC subCs nMsg
        return ()
    _ -> return ()

handleBroadcast :: EventTuple b -> M.NetMessage -> IO ()
handleBroadcast (_, clientB, pEvt, client) nMsg = case M.message nMsg of
    Nothing  -> return ()
    Just msg -> do
        currentCs <- currentValue clientB
        let i = Evt.MessageInfo (Text.length msg) Set.empty
            subscribedClients = filterClient client currentCs
        _ <- pEvt $ Evt.NetEvent Evt.Broadcast client i
        writeToAllC subscribedClients nMsg
        return ()

handleTopicReq :: EventTuple b -> M.NetMessage -> IO ()
handleTopicReq (topicB, _, pEvt, client) _ = do
    currentTs <- currentValue topicB
    let topics     = Set.map Evt._topic currentTs
        i          = Evt.MessageInfo 0 topics
        binMsg     = M.messageToByteString (M.NetMessage ReceiveTopics (Just topics) Nothing)
        c          = maybeToList $ Evt.clientHandle client
    _ <- pEvt $ Evt.NetEvent Evt.AskClient client i

    forM_ c $ \c' -> BS.hPut c' binMsg >> hFlush c'
    return ()

writeToAllC :: Set Evt.ClientCon -> M.NetMessage -> IO ()
writeToAllC cs nMsg = forM_ (Set.toList cs) $ \cn -> do
    let h      = fromJust $ Evt.clientHandle (Evt._client cn)
        binMsg = sanitizedBinaryMessage nMsg cn
    BS.hPut h binMsg
    hFlush h

writeToAllR :: Set Evt.ClientCon -> M.NetMessage -> IO ()
writeToAllR cs nMsg = forM_ (Set.toList cs) $ \cn -> do
    let h      = fromJust $ Evt.clientHandle (Evt._client cn)
        binMsg = M.messageToByteString nMsg
    BS.hPut h binMsg
    hFlush h

hasTopics :: Topics -> Evt.ClientCon -> Bool
hasTopics ts cn = not $ Set.null (Set.intersection (Evt._topics cn) ts)

hasHandle :: Evt.ClientCon -> Bool
hasHandle cn = isJust $ Evt.clientHandle (Evt._client cn)

sanitizedBinaryMessage :: M.NetMessage -> Evt.ClientCon -> BS.ByteString
sanitizedBinaryMessage nMsg cn = M.messageToByteString sMsg
   where sMsg = L.over M.topicsL (fmap (Set.intersection ts)) nMsg
         ts   = Evt._topics cn

filterClient c = Set.filter (\cn -> Evt._client cn /= c)

targetedRelays :: Evt.Client -> Set Evt.ClientCon -> Set Evt.ClientCon
targetedRelays c = Set.filter keep
    where keep cn = hasHandle cn && Evt._client cn /= c && Evt.isRelay (Evt._client cn)

targetedClients :: Topics -> Evt.Client -> Set Evt.ClientCon -> Set Evt.ClientCon
targetedClients ts c = Set.filter keep
    where keep cn = hasTopics ts cn && hasHandle cn && Evt._client cn /= c

-------------------------------
-- General UI initialization --
-------------------------------
initUIs :: Evt.NetEventGetter -> Behavior (Set Evt.ClientCon) -> Opts -> IO ()
initUIs netEvent clientB options = do
    when ( opGui options ) $ GUI.init (opGuiPort options) netEvent clientB
    when (opConsole options) $ initConsoleUI netEvent

----------------
-- Console UI --
----------------
initConsoleUI :: Evt.NetEventGetter -> IO ()
initConsoleUI netEvent = do
  evtB <- stepper undefined (netEvent Evt.AnyEvent)
  onChange evtB print
