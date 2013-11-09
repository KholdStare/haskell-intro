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

chunksOf :: Int -> Vector a -> [Vector a]
chunksOf n vec
    | VEC.null vec = []
    | otherwise    = VEC.take n vec : chunksOf n (VEC.drop n vec)

toInt32 :: Integral a => a -> Int32
toInt32 = fromInteger . toInteger

-- ====================
matlabFormat :: Show a => String -> Vector a -> String
matlabFormat name vec =
    name ++ " = [\n" ++
    (unlines $ VEC.toList $ VEC.map show vec)
    ++ "];\n"

vectorNames :: [String]
vectorNames = do
    row <- [1,2]
    col <- [1,2]
    part <- ["r", "i"]
    return $ "w" ++ show row ++ show col ++ part

main = do
    binaryContents <- BS.hGetContents stdin
    let binaryBytes = BS.length binaryContents
    let chunkSize = binaryBytes `div` (8 * 4)
    case runGet getWord32List binaryContents of
        Left a -> print a
        Right wordList -> putStr
                        $ unlines
                        $ zipWith matlabFormat vectorNames 
                        $ chunksOf chunkSize
                        $ VEC.map (toInt32 . signExtend 19)
                        $ wordList

