module P2P.Commands where

import qualified Data.ByteString as B
import qualified Data.Text       as T

data Flags        = Flags { compressed :: Bool } deriving ( Show )
type Message      = T.Text
type BinaryStream = B.ByteString


data Command = SplitEdge
             | JoinEdge
             | Redirect
             | Hello
             | Message
             deriving ( Show )
