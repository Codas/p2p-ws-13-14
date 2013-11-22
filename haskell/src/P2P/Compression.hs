module P2P.Compression where

import qualified Codec.Compression.LZ4 as LZ
import qualified Data.ByteString       as BS

import           Data.Maybe

compress :: BS.ByteString -> (BS.ByteString, Bool)
compress msg = case LZ.compress msg of
					Just result -> (result, True)
					Nothing		-> (msg, False)

decompress :: BS.ByteString -> BS.ByteString
decompress msg = fromMaybe msg (LZ.decompress msg)
{-
test = do
    contents <- BS.readFile "foo.txt"
    let (result, flag) = compress contents
    print $ BS.length result
    print $ BS.length (decompress result)
-}
