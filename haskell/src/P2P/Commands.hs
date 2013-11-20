module P2P.Commands where

import qualified Data.ByteString as B
import qualified Data.Text       as T

data Flags        = Flags { compressed :: Bool } deriving ( Show )
type Topic        = T.Text
type Message      = T.Text
type BinaryStream = B.ByteString
type Topics       = [Topic]


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
             deriving ( Show )
