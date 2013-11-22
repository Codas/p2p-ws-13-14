module P2P.Messages where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.Text            as T
import           P2P.Commands
import           P2P.Protocol

data NetMessage = NetMessage {
	command :: Command,
	topics  :: Maybe Topics,
	message :: Maybe Message
} deriving (Show)

messageToByteString :: NetMessage -> BS.ByteString
messageToByteString (NetMessage cmd topics message) = cmdBS `BS.append` topicBS `BS.append` messageBS
        where topicBS             = unparseTopics topics
              (messageBS, zipped) = unparseMessage message
              cmdBS               = unparseCommand cmd zipped

byteStringToMessage :: LS.ByteString -> NetMessage
byteStringToMessage ls = result
        where mCmd@(Just cmd)   = parseCommand $ LS.head ls
              (topics, rest)    = extractTopics (LS.tail ls) mCmd
              (message, rest2)  = extractMessage (LS.tail rest) mCmd
              result = NetMessage cmd topics message

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
       where Just tTuple    =  parseTopics $ LS.tail ls
             (topics, rest) = tTuple

extractMessage :: LS.ByteString -> Maybe Command -> (Maybe Message, LS.ByteString)
extractMessage ls Nothing = (Nothing, ls)
extractMessage ls cmd     = if containsMessage cmd then (Just message, rest) else (Nothing, ls)
       where Just mTuple     = parseMessage $ LS.tail ls
             (message, rest) = mTuple