module Server where

import           Control.Applicative
import           Control.Monad
import           Options.Applicative
import           Reactive.Threepenny
import           System.IO

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS

import           P2P.Commands
import qualified P2P.Events           as Evt
import qualified P2P.GUI              as GUI
import qualified P2P.Networking       as Net
import qualified P2P.Protocol         as P

-----------------------------------
-- Command line argument parsing --
-----------------------------------
data Opts = Opts
  { opPort    :: String
  , opAddress :: Net.HostPreference
  , opConsole :: Bool
  , opGui     :: Bool }
  deriving Show

-- define possible command line arguments. Call this programm with --help
-- to get a description of every argument
serverOpts :: Parser Opts
serverOpts = Opts
     <$> strOption
         ( long "port"
        <> short 'p'
        <> value "1337"
        <> metavar "PORT"
        <> showDefault
        <> help "Port on which to listen to incoming connections." )
     <*> nullOption
         ( long "address"
        <> short 'a'
        <> value Net.HostAny
        <> metavar "HOST"
        <> reader hostReader
        <> showDefaultWith (const "Any address")
        <> help "Address to accept incoming connections from. E.g. localhost.")
     <*> switch
         ( long "console"
        <> short 'c'
        <> help "Enable console logging of connections, msg size etc." )
     <*> switch
         ( long "gui"
        <> short 'g'
        <> help "Enable web based graphical user interface. Binds do port 10000." )

-- custom reader to parse the host preferencec
hostReader :: Monad m => String -> m Net.HostPreference
hostReader s = return $ Net.Host s

-- build the command line options, including helper text and parser
opts :: ParserInfo Opts
opts = info (serverOpts <**> helper)
  ( fullDesc
 <> progDesc "Start a TCP server and listen to incomming connections"
 <> header "Server - one Server to bring them all and to the socket bind them" )


--------------------------------------------------------------
-- Main server logic. Listen, accept, send events and pring --
--------------------------------------------------------------
main :: IO ()
main = Net.withSocketsDo $ do
    options <- execParser opts

    (netEvent, pushEvent) <- Evt.getEventBus
    initUIs netEvent options

  -- listen for incomig connections.
    let address = (opAddress options)
        port    = (opPort options)
    -- TODO: server api verwenden?
    Net.listen address port $ \(listenSocket, listenAddr) -> do
        let server = Evt.Client listenAddr Nothing
            serverInfo   = Evt.MessageInfo 0 []
        pushEvent $ Evt.NetEvent Evt.Ready server serverInfo
        forever . Net.acceptFork listenSocket $ \(connHandle, remoteAddr) -> do
            let client = Evt.Client remoteAddr (Just connHandle)
            handleClient connHandle client pushEvent

-- Read content from socket handle, display it and notify system of events
handleClient :: Handle -> Evt.Client -> (Evt.NetEvent -> IO b) -> IO ()
handleClient handle client pushEvent = do
    let clientInfo = Evt.MessageInfo 0 []
    _ <- pushEvent $ Evt.NetEvent Evt.Connected client clientInfo
    clientStream <- LS.hGetContents handle
    handleProtocol clientStream
    _ <- pushEvent $ Evt.NetEvent Evt.Disconnected client clientInfo
    return ()


handleProtocol :: LS.ByteString -> IO ()
handleProtocol bs
    | LS.null bs   = return ()
    | otherwise = case P.parseHeader $ LS.head bs of
        Nothing -> return ()
        Just (cmd, flags) -> case cmd of
            Join       -> undefined
            Part       -> undefined
            AskTopics  -> undefined
            _          -> undefined

-------------------------------
-- General UI initialization --
-------------------------------
initUIs :: Evt.NetEventGetter -> Opts -> IO ()
initUIs netEvent options = do
    when ( opGui options ) $ GUI.init netEvent        -- init web GUI
    when (opConsole options) $ initConsoleUI netEvent -- init console gui

----------------
-- Console UI --
----------------
initConsoleUI :: Evt.NetEventGetter -> IO ()
initConsoleUI netEvent = do
  evtB <- stepper undefined (netEvent Evt.AnyEvent)
  onChange evtB print
