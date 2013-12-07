module P2P.Marshalling where

import           Control.Lens
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

import           P2P.Compression
import           P2P.Messages
import           P2P.Protocol

data NetMessage = NetMessage
                  { command      :: Command
                  , address      :: Maybe IP        -- 4 Bytes
                  , port         :: Maybe Port      -- 2 Bytes
                  , uuid         :: Maybe NodeID    -- 6 Bytes
                  , fromLocation :: Maybe Location  -- 1 Byte
                  , toLocation   :: Maybe Location  -- 1 Byte
	              , message      :: Maybe Content }
                  deriving (Show)

messageToByteString :: Message -> BS.ByteString
messageToByteString msg = result
        where (NetMessage cmd addr port nodeID srcLoc trgLoc content) = toNetMessage msg
              ipBS                = unparseIP addr
              portBS              = unparsePort port
              nodeIDBS            = unparseUUID nodeID
              srcLocBS            = unparseLocation srcLoc
              trgLocBS            = unparseLocation trgLoc
              contentBS           = unparseContent content
              body                = ipBS `BS.append` portBS `BS.append` nodeIDBS `BS.append` srcLocBS `BS.append` trgLocBS `BS.append` contentBS
              zipping             = BS.length body > 20
              cmdBS               = unparseCommand cmd zipping
              (compressedBody, _) = compress body
              zippedBody          = unparseLength (BS.length compressedBody) `BS.append` compressedBody
              result              = cmdBS `BS.append` (if zipping then zippedBody else body)


byteStringToMessage :: LS.ByteString -> (Maybe Message, LS.ByteString)
byteStringToMessage ls = if zipped then zippedMessage else normalMessage
        where mCmd              = parseCommand $ LS.head ls
              (Flags zipped)    = parseFlags $ LS.head ls
              zippedMessage     = handleZippedMessage (LS.tail ls) mCmd
              normalMessage     = handleNormalMessage (LS.tail ls) mCmd

handleNormalMessage :: LS.ByteString -> Maybe Command -> (Maybe Message, LS.ByteString)
handleNormalMessage ls mCmd = (fromNetMessage $ NetMessage cmd addr port nodeID srcLoc trgLoc message, messageRest)
        where (Just cmd)                  = mCmd
              (addr, addrRest)            = extractIP ls mCmd
              (port, portRest)            = extractPort addrRest mCmd
              (nodeID, nodeIDRest)        = extractUUID portRest mCmd
              (srcLoc, srcLocRest)        = extractSrcLocation nodeIDRest mCmd
              (trgLoc, trgLocRest)        = extractTrgLocation srcLocRest mCmd
              (message, messageRest)      = extractContent trgLocRest mCmd

handleZippedMessage :: LS.ByteString -> Maybe Command -> (Maybe Message, LS.ByteString)
handleZippedMessage ls mCmd = (msg, rest)
        where (Just cmd)                            = mCmd
              Just (lengthLength, compressedLength) = parseLength ls
              (decompressedBody, rest)              = decompressStream (LS.drop lengthLength ls) compressedLength
              (msg, _)                              = handleNormalMessage decompressedBody mCmd

containsUUID :: Maybe Command -> Bool
containsUUID Nothing = False
containsUUID (Just cmd) = case cmd of 
  WithContent   -> True
  _             -> False

containsAddr :: Maybe Command -> Bool
containsAddr Nothing = False
containsAddr (Just cmd) = case cmd of
  SplitEdge     -> True
  MergeEdge     -> True
  Redirect      -> True
  _             -> False

containsPort :: Maybe Command -> Bool
containsPort Nothing = False
containsPort (Just cmd) = case cmd of
  SplitEdge     -> True
  MergeEdge     -> True
  Redirect      -> True
  _             -> False

containsSrcLocation :: Maybe Command -> Bool
containsSrcLocation Nothing = False
containsSrcLocation (Just cmd) = case cmd of
  HelloCW       -> True
  HelloCCW      -> True
  SplitEdge     -> True
  WithContent   -> True
  _             -> False

containsTrgLocation :: Maybe Command -> Bool
containsTrgLocation Nothing = False
containsTrgLocation (Just cmd) = case cmd of
  HelloCW       -> True
  HelloCCW      -> True
  MergeEdge     -> True
  Redirect      -> True
  _             -> False

containsContent :: Maybe Command -> Bool
containsContent Nothing = False
containsContent (Just cmd) = case cmd of
	WithContent   -> True
	_             -> False

extractIP :: LS.ByteString -> Maybe Command -> (Maybe IP, LS.ByteString)
extractIP ls Nothing = (Nothing, ls)
extractIP ls cmd     = if containsAddr cmd then (Just addr, rest) else (Nothing, ls)
       where Just (addr, rest) = parseIP ls

extractPort :: LS.ByteString -> Maybe Command -> (Maybe Port, LS.ByteString)
extractPort ls Nothing = (Nothing, ls)
extractPort ls cmd = if containsPort cmd then (Just port, rest) else (Nothing, ls)
       where Just (port, rest) = parsePort ls

extractUUID :: LS.ByteString -> Maybe Command -> (Maybe NodeID, LS.ByteString)
extractUUID ls Nothing = (Nothing, ls)
extractUUID ls cmd = if containsUUID cmd then (Just uuid, rest) else (Nothing, ls)
       where Just (uuid, rest) = parseUUID ls

extractSrcLocation :: LS.ByteString -> Maybe Command -> (Maybe Location, LS.ByteString)
extractSrcLocation ls Nothing = (Nothing, ls)
extractSrcLocation ls cmd     = if containsSrcLocation cmd then (Just location, rest) else (Nothing, ls)
       where Just (location, rest) = parseLocation ls

extractTrgLocation :: LS.ByteString -> Maybe Command -> (Maybe Location, LS.ByteString)
extractTrgLocation ls Nothing = (Nothing, ls)
extractTrgLocation ls cmd     = if containsTrgLocation cmd then (Just location, rest) else (Nothing, ls)
       where Just (location, rest) = parseLocation ls

extractContent :: LS.ByteString -> Maybe Command -> (Maybe Content, LS.ByteString)
extractContent ls Nothing = (Nothing, ls)
extractContent ls cmd     = if containsContent cmd then (Just content, rest) else (Nothing, ls)
       where Just mTuple  = parseContent ls
             (content, rest)  = mTuple

createHelloCWMessage :: Maybe Location -> Maybe Location -> Maybe Message
createHelloCWMessage _ _ = Nothing

createHelloCCWMessage :: Maybe Location -> Maybe Location -> Maybe Message
createHelloCCWMessage (Just srcLoc) (Just trgLoc) = Just $ HelloCWMessage srcLoc trgLoc
createHelloCCWMessage _ _ = Nothing

createContentMessage :: Maybe NodeID -> Maybe Location -> Maybe Content -> Maybe Message
createContentMessage (Just nodeID) (Just loc) (Just content) = Just $ ContentMessage nodeID loc content
createContentMessage _ _ _ = Nothing

createSplitEdgeMessage :: Maybe IP -> Maybe Port -> Maybe Location -> Maybe Message
createSplitEdgeMessage (Just ip) (Just port) (Just loc) = Just $ SplitEdgeMessage ip port loc
createSplitEdgeMessage _ _ _ = Nothing

createMergeEdgeMessage :: Maybe IP -> Maybe Port -> Maybe Location -> Maybe Message
createMergeEdgeMessage (Just ip) (Just port) (Just loc) = Just $ MergeEdgeMessage ip port loc
createMergeEdgeMessage _ _ _ = Nothing

createRedirectMessage :: Maybe IP -> Maybe Port -> Maybe Location -> Maybe Message
createRedirectMessage (Just ip) (Just port) (Just loc) = Just $ RedirectMessage ip port loc
createRedirectMessage _ _ _ = Nothing

createTryLaterMessage :: Maybe Message
createTryLaterMessage = Just TryLaterMessage

createCancelMessage :: Maybe Message
createCancelMessage = Just CancelMessage

toNetMessage :: Message -> NetMessage
toNetMessage msg = case msg of
  SplitEdgeMessage addr port loc         -> NetMessage SplitEdge (Just addr) (Just port) Nothing (Just loc) Nothing Nothing
  MergeEdgeMessage addr port loc         -> NetMessage MergeEdge (Just addr) (Just port) Nothing (Just loc) Nothing Nothing
  RedirectMessage  addr port loc         -> NetMessage Redirect  (Just addr) (Just port) Nothing (Just loc) Nothing Nothing
  HelloCWMessage   srcLoc trgLoc         -> NetMessage HelloCW  Nothing Nothing Nothing (Just srcLoc) (Just trgLoc) Nothing
  HelloCCWMessage  srcLoc trgLoc         -> NetMessage HelloCCW Nothing Nothing Nothing (Just srcLoc) (Just trgLoc) Nothing 
  ContentMessage   nodeID loc content    -> NetMessage WithContent Nothing Nothing (Just nodeID) (Just loc) Nothing (Just content)
  TryLaterMessage                        -> NetMessage TryLater Nothing Nothing Nothing Nothing Nothing Nothing
  CancelMessage                          -> NetMessage Cancel Nothing Nothing Nothing Nothing Nothing Nothing

fromNetMessage :: NetMessage -> Maybe Message
fromNetMessage (NetMessage cmd addr port nodeID srcLoc trgLoc content) = case cmd of
  SplitEdge    -> createSplitEdgeMessage    addr port srcLoc
  MergeEdge    -> createMergeEdgeMessage    addr port trgLoc
  Redirect     -> createRedirectMessage     addr port trgLoc
  HelloCW      -> createHelloCWMessage      srcLoc trgLoc
  HelloCCW     -> createHelloCCWMessage     srcLoc trgLoc
  WithContent  -> createContentMessage      nodeID srcLoc content
  TryLater     -> createTryLaterMessage
  Cancel       -> createCancelMessage
