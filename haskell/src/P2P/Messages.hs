module P2P.Messages where

import qualified Data.ByteString as B
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Data.Text       as T

data Flags        = Flags { compressed :: Bool } deriving ( Show )
type Port         = String
type IP           = String
type Location     = String
type Content      = T.Text
type BinaryStream = B.ByteString

data Command = SplitEdge
             | MergeEdge
             | Redirect
             | HelloCW
             | HelloCCW
             | WithContent
             | Disconnected
             deriving ( Show )

data Message = SplitEdgeMessage      {address :: IP, port :: Port, srcLoc :: Location}
             | MergeEdgeMessage      {address :: IP, port :: Port, srcLoc :: Location}
             | RedirectMessage       {address :: IP, port :: Port, srcLoc :: Location}
             | HelloCWMessage        {srcLoc  :: Location, trgLoc :: Location}
             | HelloCCWMessage       {srcLoc  :: Location, trgLoc :: Location}
             | ContentMessage        {address :: IP, port :: Port, srcLoc :: Location, content :: Content}
             | DisconnectedMessage 
             deriving (Show, Eq)


