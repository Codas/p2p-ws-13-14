module P2P.Protocol where

import qualified Data.Bits            as Bit
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import           Data.Int
import           Data.List
import qualified Data.Text.Encoding   as TE
import qualified Data.Word            as W

import           P2P.Commands

-- Parse topics of a given ByteString.
-- ByteString must contain the length field. Topics are separated by a null-byte.
-- Any overflowing data of the ByteString is discarded.
parseTopics :: LS.ByteString -> Maybe ([Topic], LS.ByteString)
parseTopics bs = case parseBinary bs of
    Just (binary, rest) -> Just (topics, rest)
      where topics = map TE.decodeUtf8 topicBinary
            topicBinary = BS.split (0::W.Word8) (LS.toStrict binary)
    _           -> Nothing

-- Parse the Message of a given ByteString.
-- ByteString must contain the length field.
-- Any overflowing data of the ByteString is discarded.
parseMessage :: LS.ByteString -> Maybe (Message, LS.ByteString)
parseMessage bs = case parseBinary bs of
    Just (binary, rest) -> Just (TE.decodeUtf8 (LS.toStrict bs),rest)
    _                   -> Nothing

parseBinary :: LS.ByteString -> Maybe (LS.ByteString, LS.ByteString)
parseBinary bs = case parseLength bs of
    Just (lenLen, len) -> Just (bytes, rest)
      where bytes = LS.drop lenLen (LS.take len bs)
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
              _             -> Nothing

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

bytesToInt :: [W.Word8]  -> Int64
bytesToInt ws = sum $ map bitTupleMul zipBytes
  where subBytes = map fromIntegral ws
        zipBytes = zip subBytes $ reverse [0..(length ws - 1)]
        bitTupleMul (n, i) = ((2^) `fmap` (*8)) i * n

-----------------------------
-- Bit wiggling operators --
-----------------------------
to01List :: W.Word8 -> [Int]
to01List byte = map bitSet bits
  where bitSet i = if Bit.testBit byte i then 1 else 0
        bits = reverse [0..7]

from01List :: [Int] -> Integer
from01List l = sum $ map (2^) $ elemIndices 1 $ reverse l
