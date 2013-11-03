import           Control.Concurrent (forkIO)
import           Network
import qualified Network.Socket     as S
import           System.Environment (getArgs)
import           System.IO          (BufferMode (LineBuffering), Handle, hClose,
                                     hGetContents, hPutStr, hSetBuffering,
                                     stdin)

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let port = head args

  putStrLn $ "Currently listening on Port " ++ port
  putStrLn $ "Waiting for clients ..."
  server <- serverSocket port
  handleClient server


handleClient :: Socket -> IO ()
handleClient server = do
  client@(handle, _, _) <- accept server
  hSetBuffering handle LineBuffering
  putStrLn $ "New client connected: " ++ showClient client
  forkIO $ printData client
  handleClient server

printData :: (Handle, HostName, PortNumber) -> IO ()
printData client@(handle, addr, port) = do
  input <- hGetContents handle
  putStrLn $ (showClient client) ++ ": " ++ input
  hClose handle
  putStrLn $ "Client disconnected: " ++ showClient client

-- Higher LVL API
serverSocket :: String -> IO Socket
serverSocket port = do
  let intPort = read port :: Int
  listenOn $ PortNumber $ fromIntegral intPort


showClient :: (Handle, HostName, PortNumber) -> String
showClient (_, name, port) = name ++ ":" ++ show port


-- C API like
serverSocketLow :: String -> IO Socket
serverSocketLow port = do
  let hints = S.defaultHints { S.addrFlags = [S.AI_PASSIVE] }
  addrs <- S.getAddrInfo (Just hints) Nothing (Just port)
  let addr = head addrs
  sock <- S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)
  S.bind sock (S.addrAddress addr)
  S.listen sock 128
  return sock
