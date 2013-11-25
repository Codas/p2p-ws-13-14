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

---------------------
-- High level APIs --
---------------------
-- handles client connections concurrently. Modelled after Simple.TCPs
-- acceptFork but returns a handle instead of a socket.
-- The handle is also closed automatically on disconnect or failure.
acceptFork :: S.Socket -> ((Handle, S.SockAddr) -> IO ()) -> IO ThreadId
acceptFork sock fn = do
    (socket,addr) <- S.accept sock                         -- accept connection.
    handle <- S.socketToHandle socket ReadWriteMode        -- socket -> bidirectional handle.
    hSetBuffering handle (BlockBuffering Nothing)          -- buffer by blocks of data.
    hSetBinaryMode handle True
    forkIO $ E.finally (fn (handle, addr)) (hClose handle) -- fork new io thread and make sure to
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
