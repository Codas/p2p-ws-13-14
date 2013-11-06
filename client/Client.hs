import           Network
import qualified Network.Simple.TCP as TCP
import qualified Network.Socket     as S

import           Pipes
import qualified Pipes.ByteString   as PB
import qualified Pipes.Network.TCP  as P (fromSocket)
import qualified Pipes.Prelude      as P

import           System.Environment (getArgs)
import           System.IO          (BufferMode (LineBuffering), Handle,
                                     IOMode (..), hClose, hPutStr,
                                     hSetBuffering)

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
  hPutStr handle msg
  hClose handle

sendInteractive :: Handle -> IO ()
sendInteractive handle = do
  input <- getContents
  hPutStr handle input
  hClose handle

-- Higher LVL API
getServerHandle :: HostName -> String -> IO Handle
getServerHandle host port = do
  let portNumber = PortNumber $ fromIntegral (read port :: Int)
  connectTo host portNumber


-- C-like API. Needed in case the regular API turns out to be too high level.
getServerHandleLow :: HostName -> String -> IO Handle
getServerHandleLow host port = do
  addrs <- S.getAddrInfo Nothing (Just host) (Just port)
  let addr = head addrs
  sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.setSocketOption sock S.ReuseAddr 1
  S.connect sock (S.addrAddress addr)
  S.socketToHandle sock WriteMode
