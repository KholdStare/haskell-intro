import System.Environment
import Control.Monad
import Data.Char
import Data.Function

-- Dealing with input
type TextType = String

atoi :: TextType -> Maybe Int
atoi s = case reads s of
            [] -> Nothing
            [(i, rest)] -> Just i

keyMapping = [
    (2,     "abc"),
    (3,     "def"),
    (4,     "ghi"),
    (5,     "jkl"),
    (6,     "mno"),
    (7,     "pqrs"),
    (8,     "tuv"),
    (9,     "wxyz")
    ]

splitDigits :: Int -> [Int]
splitDigits num = splitDigits' [] num
    where splitDigits' acc 0 = acc
          splitDigits' acc n = splitDigits' (n `rem` 10 : acc) (n `div` 10)

-- Coming up with possible choices for each key
type CharChoices = [[Char]]

toChoices :: Int -> Maybe CharChoices
toChoices n = do
    let digits = splitDigits n
    mapM (flip lookup keyMapping) digits

tryMatch :: CharChoices -> TextType -> Bool
tryMatch choices word = and $ (length word == length choices) : charMatches
    where charMatches = zipWith elem word choices

-- Putting it all together
type Dictionary = [TextType]

readDict :: IO TextType
readDict = readFile "/etc/dictionaries-common/words"

suggest :: Dictionary -> CharChoices -> [TextType]
suggest dict choices = filter (tryMatch choices) dict'
    where lowerCase = map toLower
          dict' = filter ( (== length choices) . length ) $ map lowerCase dict

suggestForArg :: Dictionary -> TextType -> Maybe [TextType]
suggestForArg dict arg = do
    n <- atoi arg
    choices <- toChoices n
    return $ suggest dict choices

main = do
    argList <- getArgs
    dictText <- readDict
    let dict = lines dictText
    let maybeSuggestions = map (suggestForArg dict) argList
    print $ zip argList maybeSuggestions
    
