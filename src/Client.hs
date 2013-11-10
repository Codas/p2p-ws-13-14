module Client where

import           Pipes
import qualified Pipes.ByteString      as PB

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8  as UTF

import           System.IO

import           Options.Applicative

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
        <> help "Server port to connect to." )
     <*> strOption
         ( long "host"
        <> short 'h'
        <> value "127.0.0.1"
        <> metavar "HOST"
        <> showDefault
        <> help "Server address to connect to.")
     <*> nullOption
         ( long "message"
        <> short 'm'
        <> reader msgReader
        <> metavar "MESSAGE"
        <> value Nothing
        <> help "Send a message to HOST and immediately disconnect. Leave empty to send from stdin.")

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
    Net.connectTo (opHost options) (opPort options) $ \serverHandle ->
        case opMessage options of
          Nothing  -> sendInteractive serverHandle
          Just msg -> sendMsg serverHandle msg

sendMsg :: Handle -> String -> IO ()
sendMsg handle msg = do
    putStrLn msg
    B8.hPutStrLn handle ( UTF.fromString msg )

sendInteractive :: Handle -> IO ()
sendInteractive handle = do
    let sPipe = PB.toHandle handle
    runEffect $ PB.stdin >-> sPipe
