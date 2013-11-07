import           Network
import qualified Network.Socket        as S

import           Pipes
import qualified Pipes.ByteString      as PB
import qualified Pipes.Prelude         as P

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8  as UTF

import           System.Environment    (getArgs)
import           System.IO

{-
This Application is supposed to be called with two arguments:
  host  = The host to connect to, for example "127.0.0.1".
  port  = the port to bind to, for example "1337".
  [msg] = The msg to sent. If left blank, open interactive session and
          read from stdin.

This applicatioin does not have output.
-}
main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let (host:port:optArgs) = args

  serverHandle <- getServerHandle host port
  -- make sure to send after every line. BlockBuffering would propably
  -- be better for binary data.
  hSetBuffering serverHandle LineBuffering
  return serverHandle

  -- if remaining args are found, interpret this as the msg
  -- and sent it to the server, otherwies read from stdin.
  case optArgs of
    (msg:_) -> sendMsg serverHandle msg
    _       -> sendInteractive serverHandle
  hClose serverHandle

sendMsg :: Handle -> String -> IO ()
sendMsg handle msg = do
  putStrLn msg
  B8.hPutStrLn handle ( UTF.fromString msg )

sendInteractive :: Handle -> IO ()
sendInteractive handle = do
  let sPipe = PB.toHandle handle
  runEffect $ PB.stdin >-> sPipe

-- Higher LVL API
getServerHandle :: HostName -> String -> IO Handle
getServerHandle host port = do
  let portNumber = PortNumber $ fromIntegral (read port :: Int)
  connectTo host portNumber


-- C-like API. Needed in case the regular API turns out to be too high level.
-- getServerHandleLow :: HostName -> String -> IO Handle
-- getServerHandleLow host port = do
--   addrs <- S.getAddrInfo Nothing (Just host) (Just port)
--   let addr = head addrs
--   sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
--   S.setSocketOption sock S.ReuseAddr 1
--   S.connect sock (S.addrAddress addr)
--   S.socketToHandle sock WriteMode
