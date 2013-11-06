import           System.Directory
import           System.FilePath

import           Data.List
import           Data.Maybe

import           Control.Applicative
import           Control.Concurrent          (forkIO, threadDelay)
import qualified Control.Exception           as E
import           Control.Monad

import qualified Network.Simple.TCP          as TCP
import qualified Network.Socket              as S

import           FRP.Sodium
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core (set, title, ( # ), ( #+ ), ( #. ))

import           Pipes
import qualified Pipes.Prelude               as P
import qualified Pipes.Text                  as PT

import           System.Environment          (getArgs)
import           System.IO                   (BufferMode (..), Handle,
                                              IOMode (..), hClose, hGetContents,
                                              hSetBuffering)


main :: IO ()
main = TCP.withSocketsDo $ do
  args <- getArgs
  let port = head args

  (netEvent, pushEvent)  <- mkServerEvent
  when ( argsVerbose args ) $ do
    _ <- forkIO $ initGui netEvent
    return ()

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

handleClient :: Handle -> TCP.SockAddr -> (NetEvent -> Reactive ()) -> IO ()
handleClient handle clientAddr pushEvent = do
  let cPipe = PT.fromHandle handle
  sync $ pushEvent $ NetEvent Connected clientAddr Nothing
  runEffect $ cPipe >-> PT.stdoutLn
  hClose handle
  sync $ pushEvent $ NetEvent Disconnected clientAddr Nothing

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
      sockAddr = (S.SockAddrInet ( fromIntegral intPort ) S.iNADDR_ANY)

  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.setSocketOption sock S.ReuseAddr 1
  S.bind sock sockAddr
  S.listen sock 5
  return (sock, sockAddr)


----------------------------
-- Experimental GUI stuff --
----------------------------

initGui :: ( NetEventType -> Event NetEvent ) -> IO ()
initGui getEvt = do
  let static = "Root"
      conDisEvts = merge (getEvt Connected) (getEvt Disconnected)
  sync $ do
    clients <- accum [] ( accClients <$> conDisEvts )
    listen (value clients) print
  UI.startGUI UI.defaultConfig
    { UI.tpPort       = 10000
    , UI.tpStatic     = Just static
    } setupUI
  return ()

setupUI :: UI.Window -> UI.UI ()
setupUI  window = void $ do
  return window # set title "Server Monitor"

mkServerEvent :: IO( NetEventType -> Event NetEvent, NetEvent -> Reactive ())
mkServerEvent = do
  (evtDis, pushDis) <- sync newEvent
  (evtCon, pushCon) <- sync newEvent
  (evtMsg, pushMsg) <- sync newEvent
  let getEvt = mkGetEvt evtCon evtDis evtMsg
  let pushEvt = mkPushEvt pushCon pushDis pushMsg
  return (getEvt, pushEvt)

accClients :: NetEvent -> [Client] -> [Client]
accClients (NetEvent eType client _) cs =
  case eType of
    Disconnected -> delete client cs
    Connected    -> cs ++ [client]
    _            -> cs

mkGetEvt evtCon evtDis evtMsg t =
  case t of
    Disconnected -> evtDis
    Connected    -> evtCon
    Message      -> evtMsg

mkPushEvt pushCon pushDis pushMsg evt@(NetEvent t _ _) =
  case t of
    Disconnected -> pushDis evt
    Connected    -> pushCon evt
    Message      -> pushMsg evt

------------------------
-- Connection Monitor --
------------------------

type Message = String
data NetEventType = Connected | Disconnected | Message
                  deriving ( Show, Eq )
data NetEvent = NetEvent NetEventType Client (Maybe Message)
              deriving ( Show, Eq )

type Client = TCP.SockAddr

logEvt :: NetEvent -> IO ()
logEvt (NetEvent Connected    client _)     = logLn $ "Client connected: " ++ show client
logEvt (NetEvent Disconnected client _)     = logLn $ "Client disconnected: " ++ show client
logEvt (NetEvent Message client (Just msg)) = logLn $ show client ++ ": " ++ show msg
logEvt (NetEvent Message _     Nothing)         = return ()
