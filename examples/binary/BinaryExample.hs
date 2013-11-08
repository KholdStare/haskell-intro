import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.IO (stdin)

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bytes
    | BS.length bytes <= n = [bytes]
    | otherwise            = firstChunk : (chunksOf n rest)
        where (firstChunk, rest) = BS.splitAt n bytes

main = do
    binaryContents <- BS.hGetContents stdin
    putStrLn "wololo"
