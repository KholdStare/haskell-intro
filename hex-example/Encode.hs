#!/usr/bin/env runhaskell

import qualified Data.Text.Lazy.IO as LIO
import Data.Text.Lazy (Text, transpose, chunksOf)

numBanks = 8
basename = "bank_"
suffix = ".hex"


-- chunksOf splits a Text into components of length k

enumerate :: [a] -> [(Int, a)]
enumerate = Prelude.zip [0..]

-- | Take every character in Text, and encode it into 
-- | its hex code. One per line
{-toHexDump :: Text -> Text-}

-- | Given a basename, and a list of texts,
-- | write each one out to <basename><N>.<suffix>
dumpToBankFiles :: FilePath -> String -> [Text] -> IO ()
dumpToBankFiles basename suffix texts = do
    let indexesAndTexts = enumerate texts
    mapM_ (uncurry $ LIO.writeFile . makeFilepath) indexesAndTexts
    where
        makeFilepath i = basename ++ show i ++ suffix

main = do
    lazyText <- LIO.readFile "input-text.txt"
    dumpToBankFiles basename suffix $ transpose $ chunksOf numBanks lazyText

