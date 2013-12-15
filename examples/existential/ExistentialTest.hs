{-# LANGUAGE ExistentialQuantification #-}
import Data.List

-- a is monomorphic.
-- would like to have a mixed list of anything
-- that supports the "Show" typeclass
commaSeparated :: Show a => [a] -> String
commaSeparated = intercalate ", " . map show 

data Person = Person { name :: String, age :: Int }
    deriving (Show)

-- Would like to put numbers and people in the same list
somePeople :: [Person]
somePeople = [
    Person "John Doe" 25,
    Person "Stephen Hawking" 71,
    Person "Martin Odersky" 55 ]

someNumbers :: [Int]
someNumbers = [1,2,3,4]

-- Existential magic!

data AnyShow = forall a. (Show a) => AnyShow a

instance Show AnyShow where
    show (AnyShow a) = show a

main :: IO ()
main = putStrLn $ commaSeparated $ (map AnyShow someNumbers) ++ (map AnyShow somePeople)
