module P2P.Compression where

import qualified Data.ByteString as BS

compress :: BS.ByteString -> (BS.ByteString, Bool)
compress msg = (msg, True)

decompress :: BS.ByteString -> BS.ByteString
decompress msg = undefined
