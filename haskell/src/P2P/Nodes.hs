module P2P.Nodes where

import           Control.Concurrent.STM (atomically, modifyTVar', newTVarIO,
                                         readTVar)
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
import           System.IO              (Handle)

import           System.Random          (randomRIO)

import           P2P.Messages

class HasSocket a where
    getSocket :: a -> Socket

instance HasSocket Socket where
    getSocket s = s

instance HasSocket Peer where
    getSocket = _pSocket

data ProtocolState = Free
                   | Splitting
                   | Merging
                   | Joining
                   | Done
                   deriving (Show, Eq)

data Node = Node
            { _nodeID    :: NodeID
            , _location  :: Location
            , _nAddr     :: IP
            , _nPort     :: Port
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

nSocket :: Node -> (Node -> Maybe Peer) -> Maybe Socket
nSocket node getter = fmap _pSocket (getter node)

-- nodeHandle :: Node -> (Node -> Maybe Peer) -> Maybe Handle
-- nodeHandle node getter = fmap _handle (getter node)

peerLocation :: Node -> (Node -> Maybe Peer) -> Maybe Location
peerLocation node getter = fmap _pLocation (getter node)

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
forAllSockets_ node fn = mapM_ fn $ catMaybes $ fmap (nSocket node) [_cwPeer, _ccwPeer]

--------------------
-- Node generator --
--------------------
newServerID :: IO ByteString
newServerID = do
    serverID <- UUID.nextRandom
    return $ drop 10 $ toStrict (UUID.toByteString serverID)

newNodeGenerator :: NodeID -> IP -> Port -> IO ( ProtocolState -> IO Node )
newNodeGenerator serverID addr port = do
    initial <- newTVarIO (0 :: Word8)
    return $ \state -> do
        loc <- randomRIO (0, 255)
        -- loc <- atomically $ do
        --     loc <- readTVar initial
        --     modifyTVar' initial succ
        --     return loc
        return Node { _nodeID    = serverID
                    , _location  = loc
                    , _nAddr     = addr
                    , _nPort     = port
                    , _state     = state
                    , _otherPeer = Nothing
                    , _cwPeer    = Nothing
                    , _ccwPeer   = Nothing }
-- Peers --
-----------
data Peer = Peer
            { _pSocket    :: Socket
            , _pAddr      :: IP
            , _pPort      :: Port
            , _pLocation  :: Word8
            , _isReadable :: Bool }

instance Show Peer where
    show (Peer _ _ port loc _) = show loc ++ ":" ++ port


instance Eq Peer where
    (Peer sock addr port _ loc) == (Peer sock1 addr1 port1 _ loc1) =
        sock == sock1 && loc == loc1 && addr == addr1 && port == port1

pSocket :: Lens' Peer Socket
pSocket = lens _pSocket (\record v -> record { _pSocket = v })

-- pHandle :: Lens' Peer Handle
-- pHandle = lens _handle (\record v -> record { _handle = v })
