module P2P.Messages where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.Text            as T
import           P2P.Commands
import           P2P.Protocol
import           P2P.Compression

data NetMessage = NetMessage {
	command :: Command,
	topics  :: Maybe Topics,
	message :: Maybe Message
} deriving (Show)

messageToByteString :: NetMessage -> BS.ByteString
messageToByteString (NetMessage cmd topics message) = result
        where topicBS                  = unparseTopics topics
              messageBS                = unparseMessage message
              body                     = topicBS `BS.append` messageBS
              zipping                  = BS.length body > 1000
              cmdBS                    = unparseCommand cmd zipping 
              (compressedBody, zipped) = if zipping then compress body else (body, False)
              result                   = if zipping 
              	                          then cmdBS `BS.append` (unparseLength $ BS.length compressedBody) `BS.append` compressedBody
              	                          else cmdBS `BS.append` body
           

byteStringToMessage :: LS.ByteString -> (NetMessage, LS.ByteString)
byteStringToMessage ls = if zipped then handleZippedMessage (LS.tail ls) mCmd else handleNormalMessage (LS.tail ls) mCmd
        where mCmd@(Just cmd)   = parseCommand $ LS.head ls
              (Flags zipped)    = parseFlags $ LS.head ls

handleNormalMessage :: LS.ByteString -> Maybe Command -> (NetMessage, LS.ByteString)
handleNormalMessage ls command = (NetMessage cmd topics message, messageRest)
        where (Just cmd)              = command
              (topics, topicRest)     = extractTopics ls command
              (message, messageRest)  = extractMessage topicRest command

handleZippedMessage :: LS.ByteString -> Maybe Command -> (NetMessage, LS.ByteString)
handleZippedMessage ls command = (NetMessage cmd topics message, rest)
        where (Just cmd)                            = command
              Just (lengthLength, compressedLength) = parseLength ls
              (decompressedBody, rest)              = decompressStream (LS.drop lengthLength ls) compressedLength
              (topics, topicRest)                   = extractTopics decompressedBody command
              (message, messageRest)                = extractMessage topicRest command


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
extractTopics ls cmd     = if containsTopics cmd then (Just topics, rest) else (Nothing, ls)
       where Just tTuple    =  parseTopics ls
             (topics, rest) = tTuple

extractMessage :: LS.ByteString -> Maybe Command -> (Maybe Message, LS.ByteString)
extractMessage ls Nothing = (Nothing, ls)
extractMessage ls cmd     = if containsMessage cmd then (Just message, rest) else (Nothing, ls)
       where Just mTuple     = parseMessage ls
             (message, rest) = mTuple