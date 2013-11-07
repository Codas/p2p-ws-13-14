import qualified Data.ByteString             as BS
import           Data.List

import           Control.Applicative
import           Control.Concurrent          (ThreadId, forkIO)
import qualified Control.Exception           as E
import           Control.Monad

import qualified Network.Simple.TCP          as TCP
import qualified Network.Socket              as S

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny

import           Pipes
import qualified Pipes.ByteString            as PB
import qualified Pipes.Prelude               as P

import           System.Environment          (getArgs)
import           System.IO

{-
This Application is supposed to be called with one argument:
  port = the port to bind to, for example "1337".

Possible flags are:
  -v   = Verbose commandline output (connected clients, etc) and web GUI
         on localhost:10000.

All output goes to stdout.
-}
main :: IO ()
main = TCP.withSocketsDo $ do
  args@(port:_) <- getArgs


  (netEvent, pushEvent) <- mkServerEvent
  initUIs netEvent args

  -- listen for incomig connections.
  -- the function notation (lambda + do block) just makes it easy to ensure that
  -- all open handlers are autmatically closed on function return <=> when the
  -- client disconnects or the connection terminates / fails
  TCP.listen TCP.HostAny port $ \(listenSocket, listenAddr) -> do
    logLn $ "Waiting for clients ..."
    logLn $ "Currently listening on " ++ show listenAddr
    forever . acceptFork listenSocket $ \(connSocket, remoteAddr) ->
      handleClient connSocket remoteAddr pushEvent

  -- or for the low lvl api
  -- TODO: Verify this is still working
  -- (sock, addr) <- serverSocketLow port
  -- logLn $ "Waiting for clients ..."
  -- logLn $ "Currently listening on " ++ show addr
  -- forever . TCP.acceptFork sock $ \(connSocket, remoteAddr) -> do
  --   handleClient connSocket remoteAddr pushEvent
  --   S.close connSocket
  -- S.close sock

-- Reads client data, displays it and notifies the system of (dis)connected
-- clients
handleClient :: Handle -> Client -> (NetEvent -> IO b) -> IO b
handleClient handle clientAddr pushEvent = do
  let cPipe = PB.fromHandle handle
  pushEvent $ NetEvent Connected clientAddr Nothing
  -- pipe the input of c(lient)Pipe to stdout. Enables us to optionally
  -- inntercept the input and accumulate msg size for example. All without
  -- possibly allocating the whole message in memory
  let logPrinter = P.tee PB.stdout >-> (msgSizeEvent pushEvent clientAddr)
  runEffect $ cPipe >-> logPrinter
  pushEvent $ NetEvent Disconnected clientAddr Nothing

-------------------------------------------
-- General UI (console, event, GUI) code --
-------------------------------------------

initUIs :: NetEventGetter -> [String] -> IO ()
initUIs netEvent args = do
  when ( argsVerbose args ) >> forkIO $ initGUI netEvent  -- init web GUI
  initConsoleUI netEvent

-- acuumulates NetEvents to a list of the clients converted to strings
-- TODO: Do not accumulate in strings but rather the clients directly, in
-- order to
accClients :: NetEvent -> [String] -> [String]
accClients (NetEvent eType client _) cs =
  case eType of
    Disconnected -> Data.List.delete clientS cs
    Connected    -> cs ++ [clientS]
    _            -> cs
  where clientS = show client

msgSizeEvent fireNetEvent client = do
    content <- await
    liftIO $ fireNetEvent $ NetEvent Message client $ Just ( BS.length content )
    return ()

---------------------------
-- Event generation code --
---------------------------

-- Creates an accessor to Disconnect, Connect and Message event streams.
-- Event streams can be accessed by giving a NetEVentType to NetEventGetter,
-- and can be fired by simply passing a NetEvent to the last return value of
-- this function.
-- For example:
--   (getEvt, pushEvt) = magServerEvent
--   conEvtStream = getEvt Connected
--   pushEvt NetEvent Connected client Nothing
mkServerEvent :: IO (NetEventGetter, NetEvent -> IO ())
mkServerEvent = do
  (evtDis, pushDis) <- newEvent
  (evtCon, pushCon) <- newEvent
  (evtMsg, pushMsg) <- newEvent
  let getEvt = mkGetEvt evtCon evtDis evtMsg
      pushEvt = mkPushEvt pushCon pushDis pushMsg
  return (getEvt, pushEvt)

-- create a getter function to return an event stream for a given EventType.
mkGetEvt :: t -> t -> t -> NetEventType -> t
mkGetEvt evtCon evtDis evtMsg t =
  case t of
    Disconnected -> evtDis
    Connected    -> evtCon
    Message      -> evtMsg

-- Takes multiple evtStreams to create a function that pushes a NetEvent
-- to the right stream depending on its NetEventType.
mkPushEvt :: (NetEvent -> t) -> (NetEvent -> t) -> (NetEvent -> t) -> NetEvent -> t
mkPushEvt pushCon pushDis pushMsg evt@(NetEvent t _ _) =
  case t of
    Disconnected -> pushDis evt
    Connected    -> pushCon evt
    Message      -> pushMsg evt

type MessageSize = Int   -- just an alias to make sure what we are talking about.

-- Alias to make the type signatures more expressive and shorter.
type NetEventGetter = NetEventType -> Event NetEvent

type Client = TCP.SockAddr

data NetEventType = Connected | Disconnected | Message
                  deriving ( Show, Eq )

data NetEvent = NetEvent NetEventType Client (Maybe MessageSize)
              deriving ( Show, Eq )


-----------------------------
-- Printing and Console UI --
-----------------------------

initConsoleUI :: NetEventGetter -> IO ()
initConsoleUI netEvent = do
  conB <- stepper undefined (netEvent Connected)
  disB <- stepper undefined (netEvent Disconnected)
  msgB <- stepper undefined (netEvent Message)
  onChange conB logEvt
  onChange msgB logEvt
  onChange disB logEvt

-- is the -v flag set?
argsVerbose :: [String] -> Bool
argsVerbose args = "-v" `elem` args

-- conditionally log the msg to stdout, only if the -v flag is set
logLn :: String -> IO ()
logLn msg = do
  args <- getArgs
  when (argsVerbose args) (putStrLn msg)

-- Create log entry on Event based on the event type.
logEvt :: NetEvent -> IO ()
logEvt (NetEvent Connected    client _)     = logLn $ "Client connected: " ++ show client
logEvt (NetEvent Disconnected client _)     = logLn $ "Client disconnected: " ++ show client
logEvt (NetEvent Message client (Just size)) = logLn $ "Client " ++ show client ++ " msgSize: " ++ show size
logEvt NetEvent {}                          = logLn $ "nothing"

------------------------------
-- Connection / Socket code --
------------------------------


-- handles client connections concurrently. Modelled after Simple.TCPs
-- acceptFork but returns a handle instead of a socket.
-- The handle is also closed automatically on disconnect or failure.
acceptFork :: S.Socket -> ((Handle, S.SockAddr) -> IO ()) -> IO ThreadId
acceptFork sock fn = do
  (socket,addr) <- S.accept sock
  handle <- S.socketToHandle socket ReadWriteMode
  hSetBuffering handle (BlockBuffering Nothing)
  forkIO $ E.finally (fn (handle, addr)) (hClose handle)

-- C-like API. Needed in case the regular API turns out to be too high level.
-- TODO: Make sure this ist still working
-- serverSocketLow :: String -> IO ( S.Socket, S.SockAddr )
-- serverSocketLow port = do
--   let intPort  = read port :: Int
--       sockAddr = S.SockAddrInet ( fromIntegral intPort ) S.iNADDR_ANY
--   sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
--   S.setSocketOption sock S.ReuseAddr 1
--   S.bind sock sockAddr
--   S.listen sock 5
--   return (sock, sockAddr)

----------------------------
-- Experimental GUI stuff --
----------------------------

initGUI :: NetEventGetter -> IO ()
initGUI netEvent = do
  let static = "wwwdata"
  UI.startGUI UI.defaultConfig
    { UI.tpPort       = 10000
    , UI.tpStatic     = Just static
    } ( setupGUI netEvent )

setupGUI :: NetEventGetter -> Window -> UI ()
setupGUI netEvent window = void $ do
    -- Event setups. Not sure where exactlys this belongs at the moment.
    -- TODO: On simuletanous events, only the firs event is kept. Not good...
    let conDisEvts = unionWith const (netEvent Connected) (netEvent Disconnected)
    -- clientsB keeps track of currently connected clients
    -- The way this works is: conDisEvents :: Event(NetEvent)
    -- accClient gest mapped (<$> operator) unto every incoming event, resulting
    -- in an event :: Event ([String] -> [String]).
    -- Then accumB :: a -> Event (a -> a) -> IO (Behavior a) comes into play.
    -- It takes an initial value (empyt list) as first argument, and applys, once
    -- for each incoming Event, the 2.nd function on this list.
    -- If for example the firs event in conDisEvts is of Type Connected,
    -- the value stored in accumB at this moment would be the value it was before
    -- (cs) concatenated with the a list of the string representation of the
    -- client (clientS)
    clientsB <- accumB [] ( accClients <$> conDisEvts )
    return window # set title "Server Monitor"
    cv <- mkElement "pre" #. "clientsView"
    getBody window #+ [element cv]
    -- Update the clientView on changes to the current list of clients
    element cv # sink text ( concatClients <$> clientsB )
  where concatClients = concat . \s -> intersperse ", " s
