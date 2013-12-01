module P2P.Commands where

import qualified Data.ByteString as B
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Data.Text       as T

data Flags        = Flags { compressed :: Bool } deriving ( Show )
type Topic        = T.Text
type Message      = T.Text
type BinaryStream = B.ByteString
type Topics       = Set Topic


data Command = Join
             | Part
             | AskTopics
             | ReceiveTopics
             | Message
             | Binary
             | Broadcast
             -- Admin commands
             | Close
             | Delete
             | Kick
             | Statistics
             | Relay
             | Client
             deriving ( Show )
