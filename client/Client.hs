import           Network
import qualified Network.Socket        as S

import           Pipes
import qualified Pipes.ByteString      as PB
import qualified Pipes.Prelude         as P

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8  as UTF

import           System.Environment    (getArgs)
import           System.IO             (BufferMode (LineBuffering), Handle,
                                        hClose, hSetBuffering)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let (host:port:optArgs) = args

  serverHandle <- getServerHandle host port
  hSetBuffering serverHandle LineBuffering
  return serverHandle

  case optArgs of
    (msg:_) -> sendMsg serverHandle msg
    _       -> sendInteractive serverHandle

sendMsg :: Handle -> String -> IO ()
sendMsg handle msg = do
  putStrLn msg
  B8.hPutStrLn handle ( UTF.fromString msg )
  hClose handle

sendInteractive :: Handle -> IO ()
sendInteractive handle = do
  let sPipe = PB.toHandle handle
  runEffect $ PB.stdin >-> sPipe
  hClose handle

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
