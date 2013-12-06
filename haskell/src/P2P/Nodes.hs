module P2P.Nodes where

import           Control.Concurrent.STM (atomically, modifyTVar, newTVarIO,
                                         readTVarIO)
import           Control.Lens
import           Control.Monad          (mapM_)
import           Control.Monad.Loops    (andM)

import           Data.ByteString        (ByteString, drop)
import           Data.ByteString.Lazy   (toStrict)
import           Data.Maybe             (catMaybes)
import qualified Data.UUID              as UUID
import qualified Data.UUID.V4           as UUID
import           Data.Word              (Word8)
import           Network.Socket         (Socket)
import           Prelude                hiding (drop)

import           P2P.Messages

data ProtocolState = Free
                   | Splitting
                   | Merging
                   | Done
                     deriving (Show, Eq)

data Node = Node
            { _nodeID    :: ByteString
            , _location  :: Location
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

nodeLocation :: Node -> (Node -> Maybe Peer) -> Maybe Location
nodeLocation node getter = fmap _peerLocation (getter node)

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

--------------------
-- Node generator --
--------------------
newServerID :: IO ByteString
newServerID = do
    serverID <- UUID.nextRandom
    return $ drop 10 $ toStrict (UUID.toByteString serverID)

newNodeGenerator :: ByteString -> IO ( IO Node )
newNodeGenerator serverID = do
    initial <- newTVarIO (0 :: Word8)
    return $ do
        val <- readTVarIO initial
        atomically $ modifyTVar initial succ
        return Node { _nodeID    = serverID
                    , _location  = val
                    , _state     = Free
                    , _otherPeer = Nothing
                    , _cwPeer    = Nothing
                    , _ccwPeer   = Nothing }

-----------
-- Peers --
-----------
data Peer = Peer
            { _socket       :: Socket
            , _isReadable   :: Bool
            , _peerLocation :: Word8 }
            deriving ( Show )

instance Eq Peer where
    (Peer sock _ loc) == (Peer sock1 _ loc1) = sock == sock1 && loc == loc1

socket :: Lens' Peer Socket
socket = lens _socket (\record v -> record { _socket = v })
