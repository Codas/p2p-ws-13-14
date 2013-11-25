module P2P.Events
    ( module P2P.Events
    , module Reactive.Threepenny
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Reactive.Threepenny
import qualified System.IO           as IO

import qualified P2P.Commands        as Com
import qualified P2P.Networking      as Net

-- Creates an accessor to Disconnect, Connect and Message event streams.
-- Event streams can be accessed by giving a NetEVentType to NetEventGetter,
-- and can be fired by simply passing a NetEvent to the last return value of
-- this function.
-- For example:
--   (getEvt, pushEvt) = magServerEvent
--   conEvtStream = getEvt Connected
--   pushEvt NetEvent Connected client Nothing
getEventBus :: IO (NetEventGetter, NetEvent -> IO ())
getEventBus = do
  (evt, pushEvt) <- newEvent
  let getEvt = mkGetEvt evt
  return (getEvt, pushEvt)

-- create a getter function to return an event stream for a given EventType.
mkGetEvt :: Event NetEvent -> NetEventType -> Event NetEvent
mkGetEvt evt t =
  case t of
    AnyEvent -> evt
    _        -> filterE (\(NetEvent typ _ _) -> typ == t) evt

type MessageSize = Int   -- just an alias to make sure what we are talking about.

-- Alias to make the type signatures more expressive and shorter.
type NetEventGetter = NetEventType -> Event NetEvent

data Client = Client
              { sockAddr     :: Net.SockAddr
              , clientHandle :: Maybe IO.Handle
              , isRelay      :: Bool
              , msgSize      :: MessageSize}

isRelayL :: Lens' Client Bool
isRelayL = lens isRelay (\c t -> c { isRelay = t })

instance Eq Client where
  (Client addr _ _ _) == (Client addr1 _ _ _) = addr == addr1

instance Ord Client where
    compare (Client a _ _ _) (Client a1 _ _ _) = compare a a1

instance Show Client where
    show (Client addr _ r size) = cType ++ show addr ++ " (" ++ show size ++ " bytes)"
        where cType = if r then "Relay " else "Client "

data NetEventType = Connected
                  | Disconnected
                  | Ready
                  | Join
                  | Part
                  | FirstJoin
                  | LastPart
                  | Message
                  | AskClient
                  | Broadcast
                  | AnyEvent
                  deriving ( Show, Eq )

data MessageInfo = MessageInfo
                   { mSize   :: Int
                   , mTopics :: Set Com.Topic}
                   deriving ( Eq )

instance Show MessageInfo where
    show (MessageInfo 0 ts )
        | Set.null ts = "Empty message to " ++ show (Set.toList ts)
        | otherwise    = "Empty message to nobody"
    show (MessageInfo s ts ) = "Message of size " ++ show s ++ " to " ++ show (Set.toList ts)

data NetEvent = NetEvent NetEventType Client MessageInfo
              deriving ( Eq )

instance Show NetEvent where
    show (NetEvent Connected    c _) = "Connected: " ++ show c
    show (NetEvent Disconnected c _) = "Disconnected: " ++ show c
    show (NetEvent Ready        c _) = "currently listening: " ++ show c
    show (NetEvent Join         c i) = show c ++ " joined: " ++ show (Set.toList $ mTopics i)
    show (NetEvent Part         c i) = show c ++ " parted: " ++ show (Set.toList $ mTopics i)
    show (NetEvent Message      c i) = show i ++ " from " ++ show c
    show (NetEvent t            c i) = "(NetEvent " ++ show t ++ " from " ++ show c ++ ". " ++ show i ++ ")"


data ClientCon = ClientCon
                  { _client :: Client
                  , _topics :: Set Com.Topic}

instance Eq ClientCon where
    (ClientCon c _)  == (ClientCon c1 _) = c == c1

instance Ord ClientCon where
    compare (ClientCon c _) (ClientCon c1 _) = compare c c1

instance Show ClientCon where
    show (ClientCon c t) = show c ++ " subscribed topics: " ++ show (Set.toList t)

topics :: Lens' ClientCon (Set Com.Topic)
topics = lens _topics (\clientCon t -> clientCon { _topics = t })

client :: Lens' ClientCon Client
client = lens _client (\clientCon t -> clientCon { _client = t })

data TopicClients = TopicClients
                  { _topic   :: Com.Topic
                  , _clients :: Set Client}
                  deriving (Show)

instance Eq TopicClients where
    (TopicClients t _)  == (TopicClients t1 _) = t == t1

instance Ord TopicClients where
    compare (TopicClients t _) (TopicClients t1 _) = compare t t1

clients :: Lens' TopicClients (Set Client)
clients = lens _clients (\topicClient t -> topicClient { _clients = t })

getClientBehavior :: NetEventGetter -> IO (Behavior (Set ClientCon))
getClientBehavior evt = accumB Set.empty ( accClientCon <$> evt AnyEvent)

getTopicBehavior :: NetEventGetter -> IO (Behavior (Set TopicClients))
getTopicBehavior evt = accumB Set.empty ( accTopicClients <$> evt AnyEvent)

accClientCon :: NetEvent -> Set ClientCon -> Set ClientCon
accClientCon (NetEvent eType c i) cns =
  case eType of
    Disconnected -> Set.delete cn cns
    Connected    -> Set.insert cn cns
    Join         -> Set.map (modTopics $ Set.union ets) cns
    Part         -> Set.map (modTopics (Set.\\ ets)) cns
    Message      -> Set.union sizedC cns
    Broadcast    -> Set.union sizedC cns
    _                -> cns
  where ets     = mTopics i
        cn = ClientCon c ets
        sizedC = Set.map incMessageSize $ Set.filter (\cn' -> _client cn' == c) cns
        incMessageSize = over client (\(Client a c r s) -> (Client a c r (s + mSize i)))
        modTopics :: (Set Com.Topic -> Set Com.Topic) -> ClientCon -> ClientCon
        modTopics f cn'
            | c == _client cn' = over topics f cn'
            | otherwise = cn'

accTopicClients :: NetEvent -> Set TopicClients -> Set TopicClients
accTopicClients (NetEvent eType c i) tcs =
  case eType of
    Join         -> Set.map (modClients $ Set.insert c) addedTopics
    Part         -> cleanup $ Set.map (modClients (Set.delete c)) tcs
    Disconnected -> cleanup $ Set.map (over clients (Set.delete c)) tcs
    _                -> tcs
  where ets         = mTopics i
        etc         = Set.map (`TopicClients` Set.empty) ets
        -- TODO: addedTopics sollte nur neue topics hinzufügen, momentan wird
        -- werden alle alten überschrieben,wenn die argumente vertauscht werden
        addedTopics = Set.union tcs etc
        cleanup     = Set.filter (\(TopicClients _ cs)  -> not $ Set.null cs)
        modClients :: (Set Client -> Set Client) -> TopicClients -> TopicClients
        modClients f tc
            | Set.member (_topic tc) ets = over clients f tc
            | otherwise = tc

accTopics :: NetEvent -> Set Com.Topic -> Set Com.Topic
accTopics (NetEvent eType c i) ts =
  case eType of
    Join         -> Set.union ts ets
    Part         -> ts Set.\\ ets
    _                -> ts
  where ets = mTopics i
