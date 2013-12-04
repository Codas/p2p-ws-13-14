module Server where

import           Control.Applicative
import           Control.Concurrent             (forkIO)
import           Control.Concurrent.Chan        (Chan, newChan, readChan,
                                                 writeChan)
import           Control.Concurrent.STM
import           Control.Exception              (catch, finally)
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Trans            (liftIO)
import qualified Data.UUID                      as UUID
import qualified Data.UUID.V4                   as UUID
import           Options.Applicative
import           Prelude                        hiding (catch)

import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LS
import qualified Data.List                      as L
import           Data.Maybe
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.IO                   as Text
import           Data.Word
import qualified Network.Simple.TCP             as Net
import qualified Network.Socket.ByteString.Lazy as NLS
import qualified Text.Read                      as R

import           P2P.Commands
import qualified P2P.Messages                   as M
import qualified P2P.Protocol                   as P

-----------------------------------
-- Command line argument parsing --
-----------------------------------
data Opts = Opts
  { opPort    :: String
  , opAddress :: Net.HostPreference
  , opRelay   :: Maybe (Net.HostName, Int)
  , opConsole :: Bool
  , opGui     :: Bool
  , opGuiPort :: Int}
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
        <> metavar "RELAY"
        <> reader hostReader
        <> showDefaultWith (const "Any address")
        <> help "Address to accept incoming connections from. E.g. localhost.")
     <*> nullOption
         ( long "relay"
        <> short 'r'
        <> value Nothing
        <> metavar "RELAY"
        <> reader relayReader
        <> showDefault
        <> help "Relay to connect to.")
     <*> switch
         ( long "console"
        <> short 'c'
        <> help "Enable console logging of connections, msg size etc." )
     <*> switch
         ( long "gui"
        <> short 'g'
        <> help "Enable web based graphical user interface. Binds do port 10000." )
     <*> nullOption
         ( long "gport"
        <> value 10000
        <> metavar "GUIPORT"
        <> reader auto
        <> showDefault
        <> help "GUI Port.")

-- custom reader to parse the host preferencec
hostReader :: Monad m => String -> m Net.HostPreference
hostReader s = return $ Net.Host s

relayReader :: Monad m => String -> m ( Maybe ( Net.HostName, Int ))
relayReader s = case R.readMaybe (tail i) :: (Maybe Int) of
    Just int -> return $ Just (string, int)
    _        -> return Nothing
  where (string, i) = break (== ':') s

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
    serverID <- newServerID
    newNode <- newNodeGenerator serverID
    let address = opAddress options
        port    = opPort options
    Net.listen address port $ \(lSock, lAddr) -> do
        chan <- newChan
        node <- newNode
        forkIO $ handleNode node chan lSock
        forever $ Net.acceptFork lSock $ \(rSock, rAddr) ->  do
            bytes <- NLS.getContents rSock
            connectionToMessages chan rSock bytes

newServerID :: IO BS.ByteString
newServerID = do
    serverID <- UUID.nextRandom
    return $ BS.drop 10 $ LS.toStrict (UUID.toByteString serverID)

newNodeGenerator :: BS.ByteString -> IO ( IO Node )
newNodeGenerator serverID = do
    initial <- newTVarIO (0 :: Word8)
    return $ do
        val <- readTVarIO initial
        atomically $ modifyTVar initial succ
        return Node { nodeID = serverID
                    , location = val
                    , cwPeer = Nothing
                    , ccwPeer = Nothing}

-- Just read every command
connectionToMessages :: Chan (M.NetMessage, Net.Socket) -> Net.Socket -> LS.ByteString -> IO ()
connectionToMessages chan socket bs
    | LS.null bs = writeChan chan (M.createDisconnectedMessage, socket)
    | otherwise  = do
        let (msg, rest) = M.byteStringToMessage bs
        writeChan chan (msg, socket)
        connectionToMessages chan socket rest


handleNode :: Node -> Chan (M.NetMessage, Net.Socket) -> Net.Socket -> IO ()
handleNode self chan sock = do
    (msg, rSock) <- readChan chan
    case M.command msg of
        SplitEdge -> undefined
        MergeEdge -> undefined
        Redirect  -> undefined
        HelloCW   -> undefined
        HelloCCW  -> undefined
        Message   -> undefined
        _         -> recurse self
  where recurse s = handleNode s chan sock


-----------------------------------
-- Protocol and client handling. --
-----------------------------------
-- handleProtocol :: EventTuple b -> LS.ByteString -> IO ()
-- handleProtocol evtTuple bs
--     | LS.null bs   = return ()
--     | otherwise = do
--         let (nMsg, rest) = M.byteStringToMessage bs
--         case M.command nMsg of
--             Join      -> undefined
--             Part      -> undefined
--             Message   -> undefined
--             Broadcast -> undefined
--             AskTopics -> undefined
--             _         -> return ()
--         handleProtocol evtTuple rest

---------------
-- Datatypes --
---------------
data Node = Node
            { nodeID   :: BS.ByteString
            , location :: Word8
            , cwPeer   :: Maybe Net.Socket
            , ccwPeer  :: Maybe Net.Socket}
            deriving ( Show, Eq )

-------------------------------
-- General UI initialization --
-------------------------------
-- initUIs :: Evt.NetEventGetter -> Behavior (Set Evt.ClientCon) -> Opts -> IO ()
-- initUIs netEvent clientB options = do
--     when ( opGui options ) $ GUI.init (opGuiPort options) netEvent clientB
--     when (opConsole options) $ initConsoleUI netEvent

----------------
-- Console UI --
----------------
-- initConsoleUI :: Evt.NetEventGetter -> IO ()
-- initConsoleUI netEvent = do
--   evtB <- stepper undefined (netEvent Evt.AnyEvent)
--   onChange evtB print
