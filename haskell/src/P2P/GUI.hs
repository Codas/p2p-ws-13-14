module P2P.GUI (
    module P2P.GUI
    ) where

import           Control.Concurrent          (forkIO)
import           Control.Monad
import           Data.List
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (value)
import           Reactive.Threepenny

import qualified P2P.Commands                as Com
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

init :: Evt.NetEventGetter -> Behavior (Set Evt.ClientCon) -> IO ()
init netEvent clientsB = do
    let static = "../wwwroot/"
    bytesB <- accumB 0 ( accMsgSize <$> netEvent Evt.AnyEvent )

    forkIO $ UI.startGUI UI.defaultConfig
      { UI.tpPort       = 10000
      , UI.tpStatic     = Just static
      , UI.tpCustomHTML = Just "index.html"
      } ( setupGUI clientsB bytesB )
    return ()

setupGUI :: (Show a) => Behavior (Set Evt.ClientCon) -> Behavior a -> Window -> UI ()
setupGUI clientsB bytesB window = void $ do
    (return window) # set title "Server Monitor"
    clientsView <- mkElement "pre" #. "clientsView"
    topicsView <- mkElement "pre" #. "topicView"
    netStat     <- UI.string "0"
    netView <- UI.new #. "netMonitor" #+ [
        string "Bytes total: "
      , element netStat ]
    let mainContent = [
          UI.h1 # set text "Server Monitor",
          UI.h2 # set text "Clients",
          element clientsView,
          UI.h2 # set text "Topics",
          element topicsView,
          element netView]
    UI.getBody window #+ layout mainContent

    -- Update the clientView on changes to the current list of clients
    element clientsView # sink text ( concatClients <$> clientsB )
    element topicsView # sink text ( concatTopics <$> clientsB )
    element netStat # sink text ( show <$> bytesB )
  where concatClients = intercalate "\n" . map show . Set.toList
        concatTopics  = intercalate ", " . map Text.unpack . topicSet
        topicSet      = Set.toList . Set.foldr Set.union Set.empty . Set.map Evt._topics

layout :: [UI Element] -> [UI Element]
layout mainContent = [UI.div #. "container" #+ [
                         UI.div #. "col-md-12" #+ mainContent ]]

accMsgSize :: Evt.NetEvent -> Evt.MessageSize -> Evt.MessageSize
accMsgSize (Evt.NetEvent eType _ info) size =
  case eType of
    Evt.Message   -> size + Evt.mSize info
    Evt.Broadcast -> size + Evt.mSize info
    _       -> size
