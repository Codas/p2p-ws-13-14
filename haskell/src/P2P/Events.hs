module P2P.Events
    ( module P2P.Events
    , module Reactive.Threepenny
    ) where

import           Control.Applicative
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
              , clientHandle :: Maybe IO.Handle }

instance Eq Client where
  c1 == c2 = show c1 == show c2

instance Ord Client where
    compare (Client a _) (Client a1 _) = compare a a1

instance Show Client where
    show (Client addr _) = show addr

data NetEventType = Connected
                  | Disconnected
                  | Ready
                  | Join
                  | Part
                  | FirstJoin
                  | LastPart
                  | Message
                  | Broadcast
                  | AnyEvent
                  deriving ( Show, Eq )

data MessageInfo = MessageInfo
                   { messageSize :: Int
                   , topics      :: Set Com.Topic}
                   deriving ( Eq )

instance Show MessageInfo where
    show (MessageInfo 0 ts )
        | Set.null ts = "Empty message to " ++ show (Set.toList ts)
        | otherwise    = "Empty message to nobody"
    show (MessageInfo s ts ) = "Message of size " ++ show s ++ " to " ++ show (Set.toList ts)

data NetEvent = NetEvent NetEventType Client MessageInfo
              deriving ( Eq )

instance Show NetEvent where
    show (NetEvent Connected    c _) = "Client connected: " ++ show c
    show (NetEvent Disconnected c _) = "Client disconnected: " ++ show c
    show (NetEvent Ready        c _) = "currently listening on " ++ show c
    show (NetEvent Join         c i) = "Client " ++ show c ++ " joined: " ++ show (topics i)
    show (NetEvent Part         c i) = "Client " ++ show c ++ " parted: " ++ show (topics i)
    show (NetEvent Message      c i) = show i ++ " from " ++ show c
    show (NetEvent t            c i) = "(NetEvent " ++ show t ++ " from " ++ show c ++ ". " ++ show i ++ ")"
