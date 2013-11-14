module P2P.Networking
    ( module P2P.Networking
    , module Network.Simple.TCP
    ) where

import           Control.Applicative
import           Control.Concurrent  (ThreadId, forkIO)
import qualified Control.Exception   as E
import qualified Network             as N
import           Network.Simple.TCP  hiding (acceptFork)
import qualified Network.Socket      as S
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


--------------------
-- Low level APIs --
--------------------
listenLow :: HostPreference -> String -> ((Socket, S.SockAddr) -> IO ()) -> IO ()
listenLow address port fn = do
    sockAddr <- getAddress address port  -- convert to a value the S. library needs.
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol -- Create TCP socket.
    S.setSocketOption sock S.ReuseAddr 1
    S.bind sock sockAddr -- bind socket to a address. Now we can accept connections.
    S.listen sock 5      -- allow backlog of 5.
    E.finally (fn (sock, sockAddr)) (S.close sock) -- pass handle to function
                                                   -- and make sure to really close
                                                   -- the socket on error

-- Convert address and port as that the TCP library accepts to an address
-- format the socket library wants.
getAddress :: HostPreference -> ServiceName -> IO SockAddr
getAddress address port = do
    -- Default to any address
    let myHints  = S.defaultHints { S.addrFlags = [S.AI_PASSIVE] }
        host = case address of
          Host h -> Just h
          _      -> Nothing
    -- get the S.addrAddress of the best match and return it into IO
    a <- head <$> S.getAddrInfo (Just myHints) host (Just port)
    return $ S.addrAddress a

connectToLow :: Show a => HostName -> a -> (Handle -> IO b) -> IO b
connectToLow host port fn = do
    addrs <- S.getAddrInfo Nothing (Just host) (Just (show port))
    let addr = head addrs
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.setSocketOption sock S.ReuseAddr 1
    S.connect sock (S.addrAddress addr)
    handle <- S.socketToHandle sock WriteMode
    E.finally (fn handle) (hClose handle) -- pass handle to function and make
