module P2P.Nodes where

import           Control.Lens
import           Control.Monad       (mapM_)
import           Control.Monad.Loops (andM)
import           Prelude

import           Data.ByteString     (ByteString)
import           Data.Maybe          (catMaybes)
import           Data.Word           (Word8)
import           Network.Socket      (Socket)

data ProtocolState = Free
                   | Splitting
                   | Merging
                   | Done
                     deriving (Show, Eq)

data Node = Node
            { _nodeID    :: ByteString
            , _location  :: Word8
            , _state     :: ProtocolState
            , _otherPeer :: Maybe Peer
            , _cwPeer    :: Maybe Peer
            , _ccwPeer   :: Maybe Peer }
            deriving ( Show, Eq )

----------------------
-- lenses for nodes --
----------------------
cwPeer :: Lens' Node (Maybe Peer)
cwPeer = lens _cwPeer (\record v -> record { _cwPeer = v })

ccwPeer :: Lens' Node (Maybe Peer)
ccwPeer = lens _ccwPeer (\record v -> record { _ccwPeer = v })

otherPeer :: Lens' Node (Maybe Peer)
otherPeer = lens _otherPeer (\record v -> record { _otherPeer = v })

protocolState :: Lens' Node ProtocolState
protocolState = lens _state (\record v -> record { _state = v })

------------------
-- Node queries --
------------------
isStarved :: Node -> Bool
isStarved node = and $ forAllPeers node _isReadable

nodeSocket :: Node -> (Node -> Maybe Peer) -> Maybe Socket
nodeSocket node getter = fmap _socket (getter node)

isBusy :: Node -> Bool
isBusy Node { _state = Free } = False
isBusy _                      = True

isFree :: Node -> Bool
isFree node = not $ isBusy node

isDone :: Node -> Bool
isDone node = _state node == Done

-------------------------
-- Generic Node helper --
-------------------------
forAllPeers :: Node -> (Peer -> b) -> [b]
forAllPeers node fn = fmap fn $ catMaybes [_cwPeer node, _ccwPeer node]

forAllSockets_ :: Monad m => Node -> (Socket -> m b) -> m ()
forAllSockets_ node fn = mapM_ fn $ catMaybes $ fmap (nodeSocket node) [_cwPeer, _ccwPeer]

-----------
-- Peers --
-----------
data Peer = Peer
            { _socket       :: Socket
            , _isReadable   :: Bool
            , _peerLocation :: Word8 }
            deriving ( Show, Eq )

socket :: Lens' Peer Socket
socket = lens _socket (\record v -> record { _socket = v })
