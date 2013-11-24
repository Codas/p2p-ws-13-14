module P2P.Messages where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.Text            as T
import           P2P.Commands
import           P2P.Compression
import           P2P.Protocol

data NetMessage = NetMessage
                  { command     :: Command
                  , topics      :: Maybe Topics
	              , message     :: Maybe Message }
                  deriving (Show)

messageToByteString :: NetMessage -> BS.ByteString
messageToByteString (NetMessage cmd ts msg) = result
        where topicBS                  = unparseTopics ts
              messageBS                = unparseMessage msg
              body                     = topicBS `BS.append` messageBS
              zipping                  = BS.length body > 1000
              cmdBS                    = unparseCommand cmd zipping
              (compressedBody, zipped) = if zipping then compress body else (body, False)
              result                   = if zipping
              	                         then cmdBS `BS.append` (unparseLength $ BS.length compressedBody) `BS.append` compressedBody
              	                         else cmdBS `BS.append` body


byteStringToMessage :: LS.ByteString -> (NetMessage, LS.ByteString)
byteStringToMessage ls = if zipped then zippedMessage else normalMessage
        where mCmd@(Just cmd)   = parseCommand $ LS.head ls
              (Flags zipped)    = parseFlags $ LS.head ls
              zippedMessage     = handleZippedMessage (LS.tail ls) mCmd
              normalMessage     = handleNormalMessage (LS.tail ls) mCmd

handleNormalMessage :: LS.ByteString -> Maybe Command -> (NetMessage, LS.ByteString)
handleNormalMessage ls mCmd = (NetMessage cmd ts msg, messageRest)
        where (Just cmd)         = mCmd
              (ts, topicRest)    = extractTopics ls mCmd
              (msg, messageRest) = extractMessage topicRest mCmd

handleZippedMessage :: LS.ByteString -> Maybe Command -> (NetMessage, LS.ByteString)
handleZippedMessage ls mCmd = (NetMessage cmd ts msg, rest)
        where (Just cmd)                            = mCmd
              Just (lengthLength, compressedLength) = parseLength ls
              (decompressedBody, rest)              = decompressStream (LS.drop lengthLength ls) compressedLength
              (ts, topicRest)                       = extractTopics decompressedBody mCmd
              (msg, messageRest)                    = extractMessage topicRest mCmd


containsTopics :: Maybe Command -> Bool
containsTopics Nothing = False
containsTopics (Just cmd) = case cmd of
	Join          -> True
	Part          -> True
	ReceiveTopics -> True
	Message       -> True
	Binary        -> True
	Delete        -> True
	_             -> False

containsMessage :: Maybe Command -> Bool
containsMessage Nothing = False
containsMessage (Just cmd) = case cmd of
	Message       -> True
	Binary        -> True
	Broadcast     -> True
	_             -> False

extractTopics :: LS.ByteString -> Maybe Command -> (Maybe Topics, LS.ByteString)
extractTopics ls Nothing = (Nothing, ls)
extractTopics ls cmd     = if containsTopics cmd then (Just ts, rest) else (Nothing, ls)
       where Just tTuple =  parseTopics ls
             (ts, rest)  = tTuple

extractMessage :: LS.ByteString -> Maybe Command -> (Maybe Message, LS.ByteString)
extractMessage ls Nothing = (Nothing, ls)
extractMessage ls cmd     = if containsMessage cmd then (Just msg, rest) else (Nothing, ls)
       where Just mTuple  = parseMessage ls
             (msg, rest)  = mTuple
