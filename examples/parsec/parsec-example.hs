
import Text.Parsec.String
import Text.Parsec
{-import Text.Parsec.Char-}
{-import Text.Parsec.Combinator-}

import Data.Char
import Data.List

decDigit :: Parser Int
decDigit = do
    numChar <- digit
    return $ digitToInt numChar

decNumber :: Parser Int
decNumber = do
    digitList <- many1 $ decDigit
    return $ foldl' (\acc val -> acc * 10 + val) 0 digitList

tuple :: Parser (Int,Int)
tuple = do
    char '('
    first <- decNumber
    char ','
    second <- decNumber
    char ')'
    return (first, second)
    
tuple' :: Parser (Int,Int)
tuple' = between (char '(' ) (char ')' ) $ do
    first <- decNumber
    char ','
    second <- decNumber
    return (first, second)

tuple'' :: Parser (Int,Int)
tuple'' = between (char '(' ) (char ')' ) $ do
    many $ char ' '
    first <- decNumber
    many $ char ' '
    char ','
    many $ char ' '
    second <- decNumber
    many $ char ' '
    return (first, second)

main = print $ parse tuple "test_string" "(1234,3465)"
