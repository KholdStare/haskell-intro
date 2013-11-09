#!/usr/bin/env runhaskell

-- binary serialization
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Serialize.Get
import Data.Word

-- vector storage
import qualified Data.Vector as VEC
import Data.Vector (Vector)

-- bit manipulation
import Data.Bits
import Data.Int

import System.IO (stdin)

type BitSize = Int

signExtend :: Bits a => BitSize -> a -> a
signExtend size num = if testBit num leftmost
                      then maskedNum .|. mask
                      else maskedNum
    where leftmost = size - 1
          mask = complement $ (1 `shiftL` size) - 1
          maskedNum = num .&. (complement mask)

getWord32List :: Get (Vector Word32)
getWord32List = do
    size <- remaining
    VEC.generateM (size `div` 4) (const getWord32host)

toInt32 :: Integral a => a -> Int32
toInt32 = fromInteger . toInteger

main = do
    binaryContents <- BS.hGetContents stdin
    let binaryBytes = BS.length binaryContents
    case runGet getWord32List binaryContents of
        Left a -> print a
        Right wordList -> putStr
                        $ unlines
                        $ map (show . toInt32 . signExtend 19)
                        $ VEC.toList
                        $ wordList

