module Client where

import           Options.Applicative
import           System.IO

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8  as UTF
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.IO          as Text

import           P2P.Commands
import qualified P2P.Messages          as M
import qualified P2P.Networking        as Net

data Opts = Opts
    { opPort    :: Int
    , opHost    :: String
    , opMessage :: Maybe String }
    deriving Show

-----------------------------------
-- Command line argument parsing --
-----------------------------------
serverOpts :: Parser Opts
serverOpts = Opts
     <$> option
         ( long "port"
        <> short 'p'
        <> value 1337
        <> metavar "PORT"
        <> showDefault
        <> help "Port on which to listen to incoming connections." )
     <*> strOption
         ( long "host"
        <> short 'h'
        <> value "127.0.0.1"
        <> metavar "HOST"
        <> showDefault
        <> help "Address to accept incoming connections from.")
     <*> nullOption
         ( long "message"
        <> short 'm'
        <> reader msgReader
        <> metavar "MESSAGE"
        <> value Nothing
        <> help "Send a one-off message to HOST. Leave empty to send from stdin.")

-- Maybe message reader so we can sanely check for this option
msgReader :: Monad m => String -> m ( Maybe String )
msgReader s = return $ Just s

-- build the command line options, including helper text and parser
opts :: ParserInfo Opts
opts = info (serverOpts <**> helper)
  ( fullDesc
 <> progDesc "Establish a TCP connection to a given HOST on PORT." )

---------------------------------------------------------------
-- Main client logic. Connect and send message or from stdin --
---------------------------------------------------------------
main :: IO ()
main = Net.withSocketsDo $ do
    options <- execParser opts
    Net.connectTo (opHost options) (opPort options) $ \(serverHandle, _) ->
        case opMessage options of
          Nothing  -> sendInteractive serverHandle
          Just msg -> sendMsg serverHandle $ Text.pack msg

-- send message directly
sendMsg :: Handle -> Text.Text -> IO ()
sendMsg handle msg = do
    Text.putStrLn msg
    BS.hPut handle binary
    where binary = M.messageToByteString netMessage
          netMessage = M.NetMessage Broadcast Nothing (Just msg)

-- send from stdin
sendInteractive :: Handle -> IO ()
sendInteractive handle = do
    line <- Text.hGetLine stdin
    let netMessage = M.NetMessage Broadcast Nothing (Just line)
    BS.hPut handle $ M.messageToByteString netMessage
    sendInteractive handle
