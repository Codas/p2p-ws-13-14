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
    forkIO $ E.finally (fn (handle, addr)) (hClose handle) -- fork new io thread and make sure to
                                                           -- close everything after we are done.

-- Higher LVL API to connect to a remote server
connectTo :: HostName -> Int -> (Handle -> IO b) -> IO b
connectTo host port fn = do
    handle <- N.connectTo host portNumber -- connect to host and assign handel.
    hSetBuffering handle LineBuffering    -- transmit every line (good for stdin).
    E.finally (fn handle) (hClose handle) -- pass handle to function and make
                                          -- sure to close the handle after we
                                          -- are done or in cass of errors.
  where portNumber = N.PortNumber $ fromIntegral port
