#!/usr/bin/env runhaskell

import Data.Char

import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy (Text, transpose, chunksOf)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int
-- TODO: Hide prelude?

numBanks = 8
basename = "bank_"
suffix = ".hex"

enumerate :: [a] -> [(Int, a)]
enumerate = Prelude.zip [0..]

-- | Take every character in Text, and encode it into 
-- | its hex code. One per line
toHexDump :: Text -> Text
toHexDump = LT.concatMap charToHexCode
    where charToHexCode = LT.cons '\n' . toLazyText . hexadecimal . ord
    -- TODO: see how to do it with builders

-- Data.Char                  ord :: Char -> Int
-- Data.Text.Lazy.Builder.Int hexadecimal :: Integral a => a -> Builder
-- Data.Text.Lazy.Builder     toLazyText  :: Builder -> Text
-- Data.Text.Lazy             concatMap :: (Char -> Text) -> Text -> Text

type SuffixString = String

-- | Given a basename, and a list of texts,
-- | write each one out to <basename><N>.<suffix>
dumpToBankFiles :: FilePath -> SuffixString -> [Text] -> IO ()
dumpToBankFiles basename suffix texts = do
    let indexesAndTexts = enumerate texts
    mapM_ (uncurry $ LIO.writeFile . makeFilepath) indexesAndTexts
    where
        makeFilepath i = basename ++ show i ++ suffix

main = do
    lazyText <- LIO.readFile "input-text.txt"
    dumpToBankFiles basename suffix $ map toHexDump $ transpose $ chunksOf numBanks lazyText

