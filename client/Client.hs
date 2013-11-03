import           Network
import qualified Network.Socket     as S
import           System.Environment (getArgs)
import           System.IO          (BufferMode (LineBuffering), Handle,
                                     IOMode (..), hClose, hGetContents, hPutStr,
                                     hPutStrLn, hSetBuffering, stdin)

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
clientSocketLow :: HostName -> String -> IO Handle
clientSocketLow host port = do
  addrs <- S.getAddrInfo Nothing (Just host) (Just port)
  let addr = head addrs
  sock <- S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
  S.connect sock (S.addrAddress addr)
  S.socketToHandle sock WriteMode
