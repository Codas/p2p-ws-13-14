module Server where

import qualified Data.ByteString     as BS

import           Control.Applicative
import           Control.Monad
import           Options.Applicative
import           Reactive.Threepenny
import           System.IO

import           Pipes
import qualified Pipes.ByteString    as PB
import qualified Pipes.Prelude       as P

import qualified P2P.Events          as Evt
import qualified P2P.GUI             as GUI
import qualified P2P.Networking      as Net

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
    Net.listen address port $ \(listenSocket, listenAddr) -> do
        pushEvent $ Evt.NetEvent Evt.Ready listenAddr 0
        forever . Net.acceptFork listenSocket $ \(connHandle, remoteAddr) ->
            handleClient connHandle remoteAddr pushEvent

-- Read content from socket handle, displak it and notify system of events
handleClient :: Handle -> Net.SockAddr -> (Evt.NetEvent -> IO b) -> IO b
handleClient handle clientAddr pushEvent = do
    pushEvent $ Evt.NetEvent Evt.Connected clientAddr 0
    runEffect $ clientStream >-> logPrinter
    pushEvent $ Evt.NetEvent Evt.Disconnected clientAddr 0
  where clientStream    = PB.fromHandle handle
        logPrinter      = P.tee msgSizeConsumer >-> PB.stdout
        msgSizeConsumer = mkMsgSizeConsumer pushEvent clientAddr

-- Build message size consumer. Basic pipes consumer that fires an msg size event
-- every time a chunk of data arrives.
mkMsgSizeConsumer :: MonadIO m => (Evt.NetEvent -> IO a) -> Net.SockAddr ->
                     Proxy () PB.ByteString y' y m ()
mkMsgSizeConsumer fireNetEvent client = do
    content <- await
    liftIO $ fireNetEvent $ Evt.NetEvent Evt.Message client $ BS.length content
    mkMsgSizeConsumer fireNetEvent client

-------------------------------
-- General UI initialization --
-------------------------------
initUIs :: Evt.NetEventGetter -> Opts -> IO ()
initUIs netEvent options = do
    when ( opGui options ) $ GUI.init netEvent  -- init web GUI
    when (opConsole options) $ initConsoleUI netEvent   -- init console gui

----------------
-- Console UI --
----------------
initConsoleUI :: Evt.NetEventGetter -> IO ()
initConsoleUI netEvent = do
  evtB <- stepper undefined (netEvent Evt.AnyEvent)
  onChange evtB print
