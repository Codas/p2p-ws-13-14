module P2P.Compression where

import qualified Codec.Compression.Snappy      as Comp
import qualified Codec.Compression.Snappy.Lazy as CompL
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LS
import           Data.Int

compress :: BS.ByteString -> BS.ByteString
compress = Comp.compress

decompress :: BS.ByteString -> BS.ByteString
decompress = Comp.decompress

decompressStream :: LS.ByteString -> Int64 -> (LS.ByteString, LS.ByteString)
decompressStream bs len = (decompressed, rest)
  where decompressed = CompL.decompress $ LS.take len bs
        rest         = LS.drop len bs

{-
test = do
    contents <- BS.readFile "foo.txt"
    let (result, flag) = compress contents
    print $ BS.length result
    print $ BS.length (decompress result)
-}
