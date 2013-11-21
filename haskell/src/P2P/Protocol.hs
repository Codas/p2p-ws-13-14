module P2P.Protocol where

import qualified Data.Bits          as Bit
import qualified Data.ByteString    as BS
import           Data.List
import qualified Data.Text.Encoding as TE
import qualified Data.Word          as W
import           P2P.Commands

parseTopics :: BS.ByteString -> Maybe [Topic]
parseTopics bs = case parseBinary bs of
    Just binary -> Just $ map TE.decodeUtf8 $ BS.split (0::W.Word8) binary
    _           -> Nothing

parseMessage :: BS.ByteString -> Maybe Message
parseMessage bs = case parseBinary bs of
    Just binary -> Just $ TE.decodeUtf8 binary
    _           -> Nothing

parseBinary :: BS.ByteString -> Maybe BS.ByteString
parseBinary bs = case parseLength bs of
    Just (lenLen, len) -> Just (BS.drop lenLen (BS.take len bs))
    _ -> Nothing

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
parseLength :: BS.ByteString -> Maybe (Int, Int)
parseLength bs = case BS.length bs of
    0 -> Nothing
    _ -> Just (lenLen, bytesToInt lengthBytes)
  where firstByte   = BS.head bs
        lastBytes   = BS.unpack $ BS.tail $ BS.take lenLen bs
        lenLen      = 1 + fromIntegral (Bit.shiftR firstByte 5) :: Int
        lengthBytes = Bit.clearBit (Bit.clearBit  (Bit.clearBit firstByte 5) 6) 7 : lastBytes

bytesToInt :: [W.Word8]  -> Int
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
