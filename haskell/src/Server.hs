module Main where

import           Control.Applicative
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (newChan, readChan, writeChan)
import           Control.Concurrent.STM
import           Control.Exception       (catch, finally)
import qualified Control.Lens            as L
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Trans     (liftIO)
import qualified Data.UUID               as UUID
import qualified Data.UUID.V4            as UUID
import           Options.Applicative
import           Prelude                 hiding (catch)
import           System.IO

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LS
import qualified Data.List               as L
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.IO            as Text
import           Data.Word
import qualified Network.Simple.TCP      as Net
import qualified Text.Read               as R

import           P2P.Commands
import qualified P2P.Messages            as M
import qualified P2P.Protocol            as P

-----------------------------------
-- Command line argument parsing --
-----------------------------------
data Opts = Opts
  { opPort    :: String
  , opAddress :: Net.HostPreference
  , opGui     :: Bool}
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
     <*> switch
         ( long "gui"
        <> short 'g'
        <> help "Enable web based graphical user interface. Binds do port 10000." )

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
        node <- newNode
        chan <- newChan
        forever . Net.acceptFork lSock $ \(rHandle, rAddr) -> do
            connectionToMessages node

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
        return $ Node serverID val Nothing Nothing

-- -- Read content from socket handle, display it and notify system of events
connectionToMessages = undefined

handlePeer = undefined

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
            { peerID   :: BS.ByteString
            , location :: Word8
            , cwPeer   :: Maybe Net.Socket
            , ccwPeer  :: Maybe Net.Socket}
            deriving ( Show, Eq )

-------------------------------
-- General UI initialization --
-------------------------------
-- initUIs :: Evt.NetEventGetter -> Behavior (Set Evt.ClientCon) -> Opts -> IO ()
-- initUIs netEvent clientB options = initConsoleUI netEvent

----------------
-- Console UI --
----------------
-- initConsoleUI :: Evt.NetEventGetter -> IO ()
-- initConsoleUI netEvent = do
--   evtB <- stepper undefined (netEvent Evt.AnyEvent)
--   onChange evtB print
