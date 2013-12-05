module P2P.Protocol where

import qualified Data.Bits            as Bit
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import           Data.Char
import           Data.Int
import           Data.List
import qualified Data.List.Split      as SP          
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Word            as W
import           Numeric

import           P2P.Messages

-- Parse the Message of a given ByteString.
-- ByteString must contain the length field.
-- Any overflowing data of the ByteString is discarded.
parseContent :: LS.ByteString -> Maybe (Content, LS.ByteString)
parseContent bs = case parseBinary bs of
    Just (binary, rest) -> Just (TE.decodeUtf8 (LS.toStrict binary),rest)
    _           -> Nothing

unparseContent :: Maybe Content -> BS.ByteString
unparseContent Nothing = BS.empty
unparseContent (Just message) =  lengthBS `BS.append` messageBS
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
              (0:0:0:0:0:_) -> Just SplitEdge
              (0:0:0:0:1:_) -> Just MergeEdge
              (0:0:0:1:0:_) -> Just Redirect
              (0:0:0:1:1:_) -> Just HelloCW
              (0:0:1:0:0:_) -> Just HelloCCW
              (0:0:1:0:1:_) -> Just WithContent
              (0:0:1:1:0:_) -> Just Disconnected
              _             -> Nothing

unparseCommand :: Command -> Bool -> BS.ByteString
unparseCommand command z =
    case command of
        SplitEdge           -> createCommandByteString 0  z
        MergeEdge           -> createCommandByteString 1  z
        Redirect            -> createCommandByteString 2  z
        HelloCW             -> createCommandByteString 3  z
        HelloCCW            -> createCommandByteString 4  z
        WithContent         -> createCommandByteString 5  z
        Disconnected        -> createCommandByteString 6  z


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

unparseIP :: Maybe String -> BS.ByteString
unparseIP Nothing   = BS.empty
unparseIP (Just ip) = BS.pack ipInWords
    where ipInWords = map readWord (SP.splitOn "." ip)

parseIP :: LS.ByteString -> Maybe (String, LS.ByteString)
parseIP bs = Just (result, LS.drop 4 bs)
    where elements = take 4 $ LS.unpack bs
          ipAsList = intersperse "." $ map show elements
          result   = concat ipAsList :: String

unparsePort :: Maybe String -> BS.ByteString
unparsePort Nothing     = BS.empty 
unparsePort (Just port) = BS.pack $ intToWords portAsInt
    where portAsInt = read port :: Int

parsePort :: LS.ByteString -> Maybe (String, LS.ByteString)
parsePort ls = Just(show result, LS.drop 2 ls)
    where firstWord  = fromIntegral (LS.head ls) :: Int
          secondWord = fromIntegral (LS.head $ LS.tail ls) :: Int
          result = firstWord * 256 + secondWord

unparseLocation :: Maybe String -> BS.ByteString
unparseLocation Nothing    = BS.empty
unparseLocation (Just loc) = BS.singleton locToWord
    where locToWord = read loc :: W.Word8 

parseLocation :: LS.ByteString -> Maybe (String, LS.ByteString)
parseLocation ls = Just(result, LS.tail ls)
    where result = show $ LS.head ls


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

readWord :: String -> W.Word8
readWord i = read i :: W.Word8

createCommandByteString :: Int -> Bool -> BS.ByteString
createCommandByteString cmdNum zipEnabled = BS.pack [fromIntegral finalCmdValue :: W.Word8]
    where addZipping    = if zipEnabled then 1 else 0
          finalCmdValue = (cmdNum * 2 + addZipping) * 4