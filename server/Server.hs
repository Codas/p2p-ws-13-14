module Server where

import           Data.List

import           Control.Applicative
import           Control.Concurrent          (forkIO)
import qualified Control.Exception           as E
import           Control.Monad

import qualified Network.Simple.TCP          as TCP
import qualified Network.Socket              as S

-- import           FRP.Sodium
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           Reactive.Threepenny

import           Pipes
import qualified Pipes.ByteString            as PB
import qualified Pipes.Prelude               as P

import           System.Environment          (getArgs)
import           System.IO                   (BufferMode (..), Handle,
                                              IOMode (..), hClose, hGetContents,
                                              hPutStrLn, hSetBuffering, stderr)

main :: IO ()
main = TCP.withSocketsDo $ do
  args <- getArgs
  let port = head args

  (netEvent, pushEvent) <- mkServerEvent
  when ( argsVerbose args ) >> forkIO $ initGui netEvent

  let conDisEvts = unionWith const (netEvent Connected) (netEvent Disconnected)
  clients <- accumB [] ( accClients <$> conDisEvts )
  onChange clients print

  TCP.listen TCP.HostAny port $ \(listenSocket, listenAddr) -> do
    logLn $ "Waiting for clients ..."
    logLn $ "Currently listening on " ++ show listenAddr
    forever . acceptFork listenSocket $ \(connSocket, remoteAddr) ->
      handleClient connSocket remoteAddr pushEvent

  -- or
  -- (sock, addr) <- serverSocketLow port
  -- logLn $ "Waiting for clients ..."
  -- logLn $ "Currently listening on " ++ show addr
  -- forever . TCP.acceptFork sock $ \(connSocket, remoteAddr) -> do
  --   handleClient connSocket remoteAddr pushEvent
  --   S.close connSocket
  -- S.close sock

acceptFork lsock f = do
  (socket,addr) <- S.accept lsock
  handle <- S.socketToHandle socket ReadWriteMode
  hSetBuffering handle LineBuffering
  forkIO $ E.finally (f (handle, addr)) (hClose handle)

handleClient :: Handle -> Client -> (NetEvent -> IO b) -> IO b
handleClient handle clientAddr pushEvent = do
  let cPipe = PB.fromHandle handle
  pushEvent $ NetEvent Connected clientAddr Nothing
  runEffect $ cPipe >-> PB.stdout
  hClose handle
  pushEvent $ NetEvent Disconnected clientAddr Nothing

argsVerbose :: [[Char]] -> Bool
argsVerbose args = ("-v" `elem` args)

logLn :: String -> IO ()
logLn msg = do
  args <- getArgs
  when (argsVerbose args) (putStrLn msg)

-- C-like API. Needed in case the regular API turns out to be too high level.
serverSocketLow :: String -> IO ( S.Socket, S.SockAddr )
serverSocketLow port = do
  let intPort  = read port :: Int
      sockAddr = S.SockAddrInet ( fromIntegral intPort ) S.iNADDR_ANY
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.setSocketOption sock S.ReuseAddr 1
  S.bind sock sockAddr
  S.listen sock 5
  return (sock, sockAddr)


----------------------------
-- Experimental GUI stuff --
----------------------------

initGui :: NetEventGetter -> IO ()
initGui getEvt = do
  let static = "Root"
  UI.startGUI UI.defaultConfig
    { UI.tpPort       = 10000
    , UI.tpStatic     = Just static
    } ( setupUI getEvt )

setupUI :: NetEventGetter -> Window -> UI ()
setupUI getEvt window = void $ do
    return window # set title "Server Monitor"
    let conDisEvts = unionWith const (getEvt Connected) (getEvt Disconnected)
    clients <- accumB [] ( accClients <$> conDisEvts )
    cv1 <- mkElement "pre" #. "clientsView"

    getBody window #+ [element cv1]
    element cv1 # sink text ( concatClients <$> clients )
  where concatClients = concat . \s -> intersperse ", " s

mkServerEvent :: IO (NetEventGetter, NetEvent -> IO ())
mkServerEvent = do
  (evtDis, pushDis) <- newEvent
  (evtCon, pushCon) <- newEvent
  (evtMsg, pushMsg) <- newEvent
  let getEvt = mkGetEvt evtCon evtDis evtMsg
  let pushEvt = mkPushEvt pushCon pushDis pushMsg
  return (getEvt, pushEvt)

-- accClients :: NetEvent -> [Client] -> [Client]
accClients (NetEvent eType client _) cs =
  case eType of
    Disconnected -> Data.List.delete clientS cs
    Connected    -> cs ++ [clientS]
    _            -> cs
  where clientS = show client

mkGetEvt evtCon evtDis evtMsg t =
  case t of
    Disconnected -> evtDis
    Connected    -> evtCon
    Message      -> evtMsg

mkPushEvt :: (NetEvent -> t) -> (NetEvent -> t) -> (NetEvent -> t) -> NetEvent -> t
mkPushEvt pushCon pushDis pushMsg evt@(NetEvent t _ _) =
  case t of
    Disconnected -> pushDis evt
    Connected    -> pushCon evt
    Message      -> pushMsg evt

------------------------
-- Connection Monitor --
------------------------

type Message = String
type NetEventGetter = NetEventType -> Event NetEvent
data NetEventType = Connected | Disconnected | Message
                  deriving ( Show, Eq )
data NetEvent = NetEvent NetEventType Client (Maybe Message)
              deriving ( Show, Eq )

type Client = TCP.SockAddr

logEvt :: NetEvent -> IO ()
logEvt (NetEvent Connected    client _)     = logLn $ "Client connected: " ++ show client
logEvt (NetEvent Disconnected client _)     = logLn $ "Client disconnected: " ++ show client
logEvt (NetEvent Message client (Just msg)) = logLn $ show client ++ ": " ++ show msg
logEvt NetEvent {}                          = return ()
