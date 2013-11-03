import           Control.Concurrent (forkIO)
import           Control.Monad
import           Network
import qualified Network.Socket     as S
import           System.Environment (getArgs)
import           System.IO          (BufferMode (LineBuffering), Handle, hClose,
                                     hGetContents, hSetBuffering)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let port = head args

  logLn $ "Currently listening on Port " ++ port
  logLn $ "Waiting for clients ..."
  server <- serverSocket port
  handleClient server

handleClient :: Socket -> IO ()
handleClient server = do
  client@(handle, _, _) <- accept server
  hSetBuffering handle LineBuffering
  logLn $ "New client connected: " ++ showClient client
  forkIO $ printData client
  handleClient server

printData :: (Handle, HostName, PortNumber) -> IO ()
printData client@(handle, addr, port) = do
  let clientStr = (showClient client) ++ ": "
  input <- hGetContents handle
  -- logLn $ unlines $ map (clientStr++) (lines input)
  logLn input
  hClose handle
  logLn $ "Client disconnected: " ++ showClient client

-- Higher LVL API
serverSocket :: String -> IO Socket
serverSocket port = do
  let intPort = read port :: Int
  listenOn $ PortNumber $ fromIntegral intPort

-- |Given a client tuple (like Network.accept returns), returns the host and port
-- in host:port style.
showClient :: (Handle, HostName, PortNumber) -> String
showClient (_, name, port) = name ++ ":" ++ show port

logLn :: String -> IO ()
logLn msg = do
  args <- getArgs
  when ("-v" `elem` args)
    (putStrLn msg)

-- C-like API. Needed in case the regular API turns out to be too high level.
serverSocketLow :: String -> IO Socket
serverSocketLow port = do
  let hints = S.defaultHints { S.addrFlags = [S.AI_PASSIVE] }
  addrs <- S.getAddrInfo (Just hints) Nothing (Just port)
  let addr = head addrs
  sock <- S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
  S.bind sock (S.addrAddress addr)
  S.listen sock 128
  return sock
