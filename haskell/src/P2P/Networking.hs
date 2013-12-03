module P2P.Networking
    ( module P2P.Networking
    , module Network.Simple.TCP
    ) where

import           Control.Concurrent (ThreadId, forkIO)
import qualified Control.Exception  as E
import qualified Network            as N
import           Network.Simple.TCP hiding (acceptFork)
import qualified Network.Socket     as S
import           System.IO
                                                           -- close everything after we are done.
connectTo :: Show a => HostName -> a -> ((Handle, S.SockAddr) -> IO b) -> IO b
connectTo host port fn = do
    addrs <- S.getAddrInfo Nothing (Just host) (Just (show port))
    let addr = head addrs
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.setSocketOption sock S.ReuseAddr 1
    S.connect sock (S.addrAddress addr)
    handle <- S.socketToHandle sock ReadWriteMode
    E.finally (fn (handle, S.addrAddress addr)) (hClose handle) -- pass handle to function and make
