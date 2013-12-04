module P2P.Messages where

import           Control.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import           P2P.Commands
import           P2P.Compression
import           P2P.Protocol

data NetMessage = NetMessage
                  { command      :: Command
                  , address      :: Maybe String  -- 4 Bytes
                  , port         :: Maybe String  -- 2 Bytes
                  , fromLocation :: Maybe String  -- 1 Byte
                  , toLocation   :: Maybe String  -- 1 Byte
	              , message      :: Maybe Message }
                  deriving (Show)


messageToByteString :: NetMessage -> BS.ByteString
messageToByteString (NetMessage cmd addr port srcLoc trgLoc message) = result
        where ipBS                = unparseIP addr
              portBS              = unparsePort port
              srcLocBS            = unparseLocation srcLoc
              trgLocBS            = unparseLocation trgLoc
              messageBS           = unparseMessage message
              body                = ipBS `BS.append` portBS `BS.append` srcLocBS `BS.append` trgLocBS `BS.append` messageBS
              zipping             = BS.length body > 20
              cmdBS               = unparseCommand cmd zipping
              (compressedBody, _) = compress body
              zippedBody          = unparseLength (BS.length compressedBody) `BS.append` compressedBody
              result              = cmdBS `BS.append` (if zipping then zippedBody else body)


byteStringToMessage :: LS.ByteString -> (NetMessage, LS.ByteString)
byteStringToMessage ls = if zipped then zippedMessage else normalMessage
        where mCmd              = parseCommand $ LS.head ls
              (Flags zipped)    = parseFlags $ LS.head ls
              zippedMessage     = handleZippedMessage (LS.tail ls) mCmd
              normalMessage     = handleNormalMessage (LS.tail ls) mCmd

handleNormalMessage :: LS.ByteString -> Maybe Command -> (NetMessage, LS.ByteString)
handleNormalMessage ls mCmd = (NetMessage cmd addr port srcLoc trgLoc message, messageRest)
        where (Just cmd)                  = mCmd
              (addr, addrRest)            = extractIP ls mCmd
              (port, portRest)            = extractPort addrRest mCmd
              (srcLoc, srcLocRest)        = extractSrcLocation portRest mCmd
              (trgLoc, trgLocRest)        = extractTrgLocation srcLocRest mCmd
              (message, messageRest)      = extractMessage trgLocRest mCmd

handleZippedMessage :: LS.ByteString -> Maybe Command -> (NetMessage, LS.ByteString)
handleZippedMessage ls mCmd = (msg, rest)
        where (Just cmd)                            = mCmd
              Just (lengthLength, compressedLength) = parseLength ls
              (decompressedBody, rest)              = decompressStream (LS.drop lengthLength ls) compressedLength
              (msg, _)                              = handleNormalMessage decompressedBody mCmd

containsAddr :: Maybe Command -> Bool
containsAddr Nothing = False
containsAddr (Just cmd) = case cmd of
  SplitEdge     -> True
  MergeEdge     -> True
  Redirect      -> True
  Message       -> True
  _             -> False

containsPort :: Maybe Command -> Bool
containsPort Nothing = False
containsPort (Just cmd) = case cmd of
  SplitEdge     -> True
  MergeEdge     -> True
  Redirect      -> True
  Message       -> True
  _             -> False

containsSrcLocation :: Maybe Command -> Bool
containsSrcLocation Nothing = False
containsSrcLocation (Just cmd) = case cmd of
  HelloCW       -> True
  HelloCCW      -> True
  SplitEdge     -> True
  MergeEdge     -> True
  Redirect      -> True
  Message       -> True
  _             -> False

containsTrgLocation :: Maybe Command -> Bool
containsTrgLocation Nothing = False
containsTrgLocation (Just cmd) = case cmd of
  HelloCW       -> True
  HelloCCW      -> True
  _             -> False

containsMessage :: Maybe Command -> Bool
containsMessage Nothing = False
containsMessage (Just cmd) = case cmd of
	Message       -> True
	_             -> False

extractIP :: LS.ByteString -> Maybe Command -> (Maybe IP, LS.ByteString)
extractIP ls Nothing = (Nothing, ls)
extractIP ls cmd     = if containsAddr cmd then (Just addr, rest) else (Nothing, ls)
       where Just (addr, rest) = parseIP ls

extractPort :: LS.ByteString -> Maybe Command -> (Maybe Port, LS.ByteString)
extractPort ls Nothing = (Nothing, ls)
extractPort ls cmd     = if containsPort cmd then (Just port, rest) else (Nothing, ls)
       where Just (port, rest) = parsePort ls

extractSrcLocation :: LS.ByteString -> Maybe Command -> (Maybe Location, LS.ByteString)
extractSrcLocation ls Nothing = (Nothing, ls)
extractSrcLocation ls cmd     = if containsSrcLocation cmd then (Just location, rest) else (Nothing, ls)
       where Just (location, rest) = parseLocation ls

extractTrgLocation :: LS.ByteString -> Maybe Command -> (Maybe Location, LS.ByteString)
extractTrgLocation ls Nothing = (Nothing, ls)
extractTrgLocation ls cmd     = if containsTrgLocation cmd then (Just location, rest) else (Nothing, ls)
       where Just (location, rest) = parseLocation ls

extractMessage :: LS.ByteString -> Maybe Command -> (Maybe Message, LS.ByteString)
extractMessage ls Nothing = (Nothing, ls)
extractMessage ls cmd     = if containsMessage cmd then (Just msg, rest) else (Nothing, ls)
       where Just mTuple  = parseMessage ls
             (msg, rest)  = mTuple


createHelloMessage :: Command -> Location -> Location -> NetMessage
createHelloMessage cmd srcLoc trgLoc = NetMessage cmd Nothing Nothing (Just srcLoc) (Just trgLoc) Nothing

createHelloCWMessage :: Location -> Location -> NetMessage
createHelloCWMessage = createHelloMessage HelloCW

createHelloCCWMessage :: Location -> Location -> NetMessage
createHelloCCWMessage = createHelloMessage HelloCCW

createContentMessage :: IP -> Port -> Location -> Message -> NetMessage
createContentMessage ip port loc msg = NetMessage Message (Just ip) (Just port) (Just loc) Nothing (Just msg)

createEdgeMessage :: Command -> IP -> Port -> Location -> NetMessage
createEdgeMessage cmd ip port loc = NetMessage cmd (Just ip) (Just port) (Just loc) Nothing Nothing

createSplitEdgeMessage :: IP -> Port -> Location -> NetMessage
createSplitEdgeMessage = createEdgeMessage SplitEdge

createMergeEdgeMessage :: IP -> Port -> Location -> NetMessage
createMergeEdgeMessage = createEdgeMessage MergeEdge

createRedirectMessage :: IP -> Port -> Location -> NetMessage
createRedirectMessage = createEdgeMessage Redirect

createDisconnectedMessage :: NetMessage
createDisconnectedMessage = NetMessage Disconnected Nothing Nothing Nothing Nothing Nothing
