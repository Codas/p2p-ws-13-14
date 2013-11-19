module P2P.GUI (
    module P2P.GUI
    ) where

import           Control.Concurrent          (forkIO)
import           Control.Monad
import           Data.List

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (value)
import           Reactive.Threepenny

import qualified P2P.Events                  as Evt
import qualified P2P.Networking              as Net

-------------------
-- Web GUI stuff --
-------------------
data ClientStats = ClientStats Net.SockAddr Evt.MessageSize

instance Eq ClientStats where
    (==) (ClientStats c1 _) (ClientStats c2 _) = c1 == c2

instance Show ClientStats where
  show (ClientStats client size) = show client ++ ", received bytes: " ++ show size

init :: Evt.NetEventGetter -> IO ()
init netEvent = do
    let static = "../wwwroot/"
    (clientsB, bytesB) <- getNetBehavior netEvent

    forkIO $ UI.startGUI UI.defaultConfig
      { UI.tpPort       = 10000
      , UI.tpStatic     = Just static
      , UI.tpCustomHTML = Just "index.html"
      } ( setupGUI clientsB bytesB )
    return ()

setupGUI :: (Show a, Show a1) => Behavior [a] -> Behavior a1 -> Window -> UI ()
setupGUI clientsB bytesB window = void $ do
    (return window) # set title "Server Monitor"
    clientsView <- mkElement "pre" #. "clientsView"
    netStat     <- UI.string "0"
    netView <- UI.new #. "netMonitor" #+ [
        string "Bytes total: "
      , element netStat ]
    let mainContent = [
          UI.h1 # set text "Server Monitor",
          element clientsView,
          element netView]
    UI.getBody window #+ layout mainContent

    -- Update the clientView on changes to the current list of clients
    element clientsView # sink text ( concatClients <$> clientsB )
    element netStat # sink text ( show <$> bytesB )
  where concatClients = concat . (\s -> intersperse "\n" s) . map show

layout :: [UI Element] -> [UI Element]
layout mainContent = [UI.div #. "container" #+ [
                         UI.div #. "col-md-12" #+ mainContent ]]

getNetBehavior :: MonadIO m => Evt.NetEventGetter ->
                  m (Behavior [ClientStats], Behavior Evt.MessageSize)
getNetBehavior netEvent = do
    -- clientsB keeps track of currently connected clients (as ClientStats)
    clientsB <- accumB [] ( accClientStats <$> netEvent Evt.AnyEvent)
    bytesB <- accumB 0 ( accMsgSize <$> netEvent Evt.Message )
    return (clientsB, bytesB)

-- accumulates NetEvents to a list of the clients converted to strings
-- TODO: Do not accumulate in strings but rather the clients directly?
accClients :: Evt.NetEvent -> [String] -> [String]
accClients (Evt.NetEvent eType client _) cs =
  case eType of
    Evt.Disconnected -> Data.List.delete clientS cs
    Evt.Connected    -> cs ++ [clientS]
    _            -> cs
  where clientS = show client

accClientStats :: Evt.NetEvent -> [ClientStats] -> [ClientStats]
accClientStats (Evt.NetEvent eType client msgSize) cs =
  case eType of
    Evt.Disconnected -> Data.List.delete clientS cs
    Evt.Connected    -> cs ++ [clientS]
    Evt.Message      -> map incSize cs
    _                -> cs
  where clientS = ClientStats client msgSize
        incSize stat@(ClientStats c s) = if c == client
                                         then ClientStats c $! s + msgSize
                                         else stat

accMsgSize :: Evt.NetEvent -> Evt.MessageSize -> Evt.MessageSize
accMsgSize (Evt.NetEvent eType _ msgSize) size =
  case eType of
    Evt.Message -> size + msgSize
    _       -> size
