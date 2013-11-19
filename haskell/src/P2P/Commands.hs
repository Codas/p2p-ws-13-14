module P2P.Comands where

import qualified Data.ByteString as B



data Flags        = Flags { compressed :: Bool}
type Topic        = B.ByteString
type Message      = B.ByteString
type BinaryStream = B.ByteString
type Topics       = [Topic]

data Command = Join Flags Topics
             | Part Flags Topics
             | AskTopics
             | ReceiveTopics Flags Topics
             | Message Topics Message
             | Binary Topics BinaryStream
             | Broadcast Message
             | Close Flags Topics
             | Delete Flags Topics
             | Kick Flags Topics
             | Statistics Flags Topics
