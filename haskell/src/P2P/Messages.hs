module P2P.Messages where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Data.Word       (Word8)
import           Network.Socket  (Socket)

data Flags        = Flags { compressed :: Bool } deriving ( Show )
type Port         = String
type IP           = String
type Location     = Word8
type Content      = Text
type BinaryStream = ByteString

data Command = SplitEdge
             | MergeEdge
             | Redirect
             | HelloCW
             | HelloCCW
             | TryLater
             | Cancel
             | WithContent
             deriving ( Show )

data Message = SplitEdgeMessage {address :: IP, port :: Port, srcLoc :: Location}
             | MergeEdgeMessage {address :: IP, port :: Port, srcLoc :: Location}
             | RedirectMessage  {address :: IP, port :: Port, srcLoc :: Location}
             | HelloCWMessage   {srcLoc  :: Location, trgLoc :: Location}
             | HelloCCWMessage  {srcLoc  :: Location, trgLoc :: Location}
             | ContentMessage   {address :: IP, port :: Port, srcLoc :: Location, content :: Content}
             | TryLaterMessage
             | CancelMessage
             | Shutdown
             deriving (Show, Eq)
