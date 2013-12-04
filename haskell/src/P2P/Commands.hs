module P2P.Commands where

import qualified Data.ByteString as B
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Data.Text       as T

data Flags        = Flags { compressed :: Bool } deriving ( Show )
type Port         = String
type IP           = String
type Location     = String
type Message      = T.Text
type BinaryStream = B.ByteString

data Command = SplitEdge
             | MergeEdge
             | Redirect
             | HelloCW
             | HelloCCW
             | Message
             -- Admin commands
             deriving ( Show )
