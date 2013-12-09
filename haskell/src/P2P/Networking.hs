module P2P.Networking
    ( module P2P.Networking
    , module Network.Simple.TCP
    ) where

import           Control.Concurrent (ThreadId, forkIO)
import qualified Control.Exception  as E
import qualified Network            as N
import           Network.Simple.TCP hiding (acceptFork, connectTo)
import qualified Network.Socket     as S
import           System.IO

---------------------
-- High level APIs --
---------------------
-- handles client connections concurrently. Modelled after Simple.TCPs
-- acceptFork but returns a handle instead of a socket.
-- The handle is also closed automatically on disconnect or failure.
acceptFork :: S.Socket -> ((S.Socket, S.SockAddr) -> IO ()) -> IO ThreadId
acceptFork sock fn = do
    (socket,addr) <- S.accept sock                         -- accept connection.
    -- handle <- S.socketToHandle socket ReadWriteMode        -- socket -> bidirectional handle.
    -- hSetBuffering handle NoBuffering          -- buffer by blocks of data.
    -- hSetBinaryMode handle True
    forkIO $ E.finally (fn (socket, addr)) (S.close socket)

connectTo :: String -> String -> ((Socket, S.SockAddr) -> IO b) -> IO b
connectTo host port fn = do
    addrs <- S.getAddrInfo Nothing (Just host) (Just port)
    let addr = head addrs
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    -- S.setSocketOption sock S.ReuseAddr 1
    S.connect sock (S.addrAddress addr)
    -- handle <- S.socketToHandle sock ReadWriteMode
    -- hSetBuffering handle NoBuffering          -- buffer by blocks of data.
    -- hSetBinaryMode handle True
    E.finally (fn (sock, S.addrAddress addr)) (S.close sock)
