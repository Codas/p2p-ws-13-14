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

-- handles client connections concurrently. Modelled after Simple.TCPs
-- acceptFork but returns a handle instead of a socket.
-- The handle is also closed automatically on disconnect or failure.
acceptFork :: S.Socket -> ((Handle, S.SockAddr) -> IO ()) -> IO ThreadId
acceptFork sock fn = do
    (socket,addr) <- S.accept sock
    handle <- S.socketToHandle socket ReadWriteMode
    hSetBuffering handle (BlockBuffering Nothing)
    forkIO $ E.finally (fn (handle, addr)) (hClose handle)

-- Higher LVL API
connectTo :: HostName -> Int -> (Handle -> IO b) -> IO b
connectTo host port fn = do
    handle <- N.connectTo host portNumber
    hSetBuffering handle LineBuffering
    E.finally (fn handle) (hClose handle)
  where portNumber = N.PortNumber $ fromIntegral port


-- C-like API. Needed in case the regular API turns out to be too high level.
-- TODO: Make sure this ist still working
-- listenLow :: String -> IO ( S.Socket, S.SockAddr )
-- listenLow port = do
--     let intPort  = read port :: Int
--         sockAddr = S.SockAddrInet ( fromIntegral intPort ) S.iNADDR_ANY
--     sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
--     S.setSocketOption sock S.ReuseAddr 1
--     S.bind sock sockAddr
--     S.listen sock 5
--     return (sock, sockAddr)

-- C-like API. Needed in case the regular API turns out to be too high level.
-- connectToLow :: HostName -> String -> IO Handle
-- connectToLow host port = do
--     addrs <- S.getAddrInfo Nothing (Just host) (Just port)
--     let addr = head addrs
--     sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
--     S.setSocketOption sock S.ReuseAddr 1
--     S.connect sock (S.addrAddress addr)
--     S.socketToHandle sock WriteMode
