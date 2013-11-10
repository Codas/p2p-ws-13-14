module P2P.Events
    ( module P2P.Events
    , module Reactive.Threepenny
    ) where

import           Control.Applicative

import           Reactive.Threepenny

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

type Client = Net.SockAddr

data NetEventType = Connected | Disconnected | Message | Ready | AnyEvent
                  deriving ( Show, Eq )

data NetEvent = NetEvent NetEventType Client MessageSize
              deriving ( Eq )

instance Show NetEvent where
    show (NetEvent Connected    client _) = "Client connected: " ++ show client
    show (NetEvent Disconnected client _) = "Client disconnected: " ++ show client
    show (NetEvent Message client size)   = show size ++ " bytes received from " ++ show client
    show (NetEvent Ready addr _)          = "currently listening on " ++ show addr
    show (NetEvent t a s)                 = "(NetEvent " ++ show t ++ " " ++ show a ++ " " ++ show s ++ ")"
