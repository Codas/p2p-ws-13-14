module P2P.Messages where

import           Control.Lens
import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Data.Word       (Word8)
import           Network.Socket  (Socket)

data Flags        = Flags { compressed :: Bool } deriving ( Show )
type IP           = String
type Port         = String
type NodeID       = ByteString
type Location     = Word8
type Content      = ByteString
type BinaryStream = ByteString

data Command = SplitEdge
             | MergeEdge
             | Redirect
             | HelloCW
             | HelloCCW
             | TryLater
             | Cancel
             | Shutdown
             | WithContent
             deriving ( Show )

data Message = SplitEdgeMessage {address :: IP, port :: Port, srcLoc :: Location}
             | MergeEdgeMessage {address :: IP, port :: Port, trgLoc :: Location}
             | RedirectMessage  {address :: IP, port :: Port, trgLoc :: Location}
             | HelloCWMessage   {srcLoc  :: Location, trgLoc :: Location}
             | HelloCCWMessage  {srcLoc  :: Location, trgLoc :: Location}
             | ContentMessage   {nodeID  :: NodeID,   srcLoc :: Location, content :: Content}
             | TryLaterMessage
             | CancelMessage
             | ShutdownMessage
             | InitLeaveMessage
             | SendContentMessage {nodeID :: NodeID}
             deriving (Show, Eq)

_srcLoc :: Lens' Message Location
_srcLoc = lens srcLoc (\record v -> record { srcLoc = v })
