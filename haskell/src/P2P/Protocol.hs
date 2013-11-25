module P2P.Protocol where

import qualified Data.Bits            as Bit
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import           Data.Char
import           Data.Int
import           Data.List
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Word            as W
import           Numeric

import           P2P.Commands

-- Parse topics of a given ByteString.
-- ByteString must contain the length field. Topics are separated by a null-byte.
-- Any overflowing data of the ByteString is discarded.
parseTopics :: LS.ByteString -> Maybe (Set Topic, LS.ByteString)
parseTopics bs = case parseBinary bs of
    Just (binary, rest) -> Just (Set.fromList topics, rest)
      where topics = map TE.decodeUtf8 topicBinary
            topicBinary = BS.split (0::W.Word8) (LS.toStrict binary)
    _           -> Nothing

unparseTopics :: Maybe Topics -> BS.ByteString
unparseTopics Nothing = BS.empty
unparseTopics (Just topics)
      | Set.null topics = BS.empty
      | otherwise        = lengthBS `BS.append` topicBS
    where nullByte = BS.singleton (0 :: W.Word8)
          topicBS  = BS.concat $ intersperse nullByte (map TE.encodeUtf8 $ Set.toList topics)
          lengthBS = unparseLength $ BS.length topicBS

-- Parse the Message of a given ByteString.
-- ByteString must contain the length field.
-- Any overflowing data of the ByteString is discarded.
parseMessage :: LS.ByteString -> Maybe (Message, LS.ByteString)
parseMessage bs = case parseBinary bs of
    Just (binary, rest) -> Just (TE.decodeUtf8 (LS.toStrict binary),rest)
    _           -> Nothing

unparseMessage :: Maybe Message -> BS.ByteString
unparseMessage Nothing = BS.empty
unparseMessage (Just message) =  lengthBS `BS.append` messageBS
  where messageBS  = TE.encodeUtf8 message
        lengthBS = unparseLength $ BS.length messageBS

parseBinary :: LS.ByteString -> Maybe (LS.ByteString, LS.ByteString)
parseBinary bs = case parseLength bs of
    Just (lenLen, len) -> Just (bytes, rest)
      where bytes =  LS.take len (LS.drop lenLen bs)
            rest  = LS.drop (lenLen + len) bs
    _ -> Nothing

-- Parse the header field of the protocol.
-- Only the byte (Word8) containing the flags and optcode
-- should be passed. Other bytes will return unpredictable results.
parseHeader :: W.Word8 -> Maybe (Command, Flags)
parseHeader byte = case command of
    Just c  -> Just (c, flags)
    Nothing -> Nothing
  where flags = parseFlags byte
        command = parseCommand byte

parseCommand :: W.Word8 -> Maybe Command
parseCommand byte = command
    where bits = to01List byte
          command = case bits of
              (0:0:0:0:0:_) -> Just Join
              (0:0:0:0:1:_) -> Just Part
              (0:0:0:1:0:_) -> Just AskTopics
              (0:0:0:1:1:_) -> Just ReceiveTopics
              (0:0:1:0:0:_) -> Just Message
              (0:0:1:0:1:_) -> Just Binary
              (0:0:1:1:0:_) -> Just Broadcast
              (1:0:0:0:0:_) -> Just Close
              (1:0:0:0:1:_) -> Just Delete
              (1:0:0:1:0:_) -> Just Kick
              (1:0:1:0:0:_) -> Just Statistics
              (1:1:1:1:1:_) -> Just Relay
              _             -> Nothing

unparseCommand :: Command -> Bool -> BS.ByteString
unparseCommand command z =
    case command of
        Join           -> createCommandByteString 0  z
        Part           -> createCommandByteString 1  z
        AskTopics      -> createCommandByteString 2  z
        ReceiveTopics  -> createCommandByteString 3  z
        Message        -> createCommandByteString 4  z
        Binary         -> createCommandByteString 5  z
        Broadcast      -> createCommandByteString 6  z
        Close          -> createCommandByteString 16 z
        Delete         -> createCommandByteString 17 z
        Kick           -> createCommandByteString 18 z
        Statistics     -> createCommandByteString 20 z
        Relay          -> createCommandByteString 31 False


parseFlags :: W.Word8 -> Flags
parseFlags byte = Flags { compressed = comp }
  where comp = Bit.testBit byte 2

-- Returns a Tuple of Ints, first one being the length of the length field
-- in bytes, second one being the length of the coming message / topic etc.
parseLength :: LS.ByteString -> Maybe (Int64, Int64)
parseLength bs = if LS.null (LS.take 8 bs)
                 then Nothing
                 else Just (lenLen, bytesToInt lengthBytes)
  where firstByte   = LS.head bs
        lastBytes   = LS.unpack $ LS.tail $ LS.take lenLen bs
        lenLen      = 1 + fromIntegral (Bit.shiftR firstByte 5)
        lengthBytes = Bit.clearBit (Bit.clearBit  (Bit.clearBit firstByte 5) 6) 7 : lastBytes

unparseLength :: Int -> BS.ByteString
unparseLength i = result
  where bitCount            = countBits i
        numberOfBytes       = (bitCount + 2) `div` 8 * 32 -- with shiftL 5 bits
        firstByte           = fromIntegral numberOfBytes :: W.Word8
        longValueField      = intToWords i
        headValue           = head longValueField
        needExtraBits       = any (Bit.testBit headValue) [5..7]
        finalFirstByte      = BS.singleton (if needExtraBits then firstByte else headValue + firstByte)
        finalValueField     = BS.pack (if needExtraBits then longValueField else tail longValueField)
        result              = finalFirstByte `BS.append` finalValueField

bytesToInt :: (Integral a, Num a1) => [a] -> a1
bytesToInt ws = sum $ map bitTupleMul zipBytes
  where subBytes = map fromIntegral ws
        zipBytes = zip subBytes $ reverse [0..(length ws - 1)]
        bitTupleMul (n, i) = ((2^) `fmap` (*8)) i * n

------------------------------
-- Bit wiggling operations --
------------------------------
to01List :: W.Word8 -> [Int]
to01List byte = map bitSet bits
  where bitSet i = if Bit.testBit byte i then 1 else 0
        bits = reverse [0..7]

from01List :: [Int] -> Integer
from01List l = sum $ map (2^) $ elemIndices 1 $ reverse l

-----------------------------
--      Miscellaneous      --
-----------------------------

countBits :: Int -> Int
countBits i = length $ showIntAtBase 2 intToDigit i ""

intToWords :: Int -> [W.Word8]
intToWords i = if i == 0 then []
               else intToWords (i `div` 256) ++ [fromIntegral i :: W.Word8]

createCommandByteString :: Int -> Bool -> BS.ByteString
createCommandByteString cmdNum zipEnabled = BS.pack [fromIntegral finalCmdValue :: W.Word8]
    where addZipping = if zipEnabled then 1 else 0
          finalCmdValue = (cmdNum * 2 + addZipping) * 4
