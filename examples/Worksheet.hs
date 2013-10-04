import Data.Char
import Data.List
import Data.Monoid

import Prelude hiding (head, tail)

-- Lists
-- =====

-- get first element of list
head :: [a] -> a
head (x:_) = x
-- non-exhaustive pattern match
-- -fwarn-incomplete-patterns

tail :: [a] -> [a]
tail [] = []
tail (x:xs) = xs

-- more complicated pattern miss
swap2 :: [a] -> [a]
swap2 [] = []
swap2 (x:y:rest) = (y:x:rest)
-- works for any and all structures - is generic

-- Imperative control structures
-- =============================
if' :: Bool -> a -> a -> a
if' True  t _ = t
if' False _ f = f

-- show laziness (does not evaluate error)
ifExample = if' True
                "You're safe!"
                undefined

while :: (s -> Bool) -> (s -> s) -> s -> s
while condition trans state =
    if' (condition state)
        (while condition trans (trans state))
        (state)

-- lambda functions
whileExample = while (\i -> i < 20)
                     (\i -> i * 3)
                     1

for :: (Int, Int) -> ( (Int, s) -> s ) -> s -> s
for (lower, upper) trans startState =
    snd $
    while (\(i, _) -> i < upper)
          (\(i, s) -> (i + 1, trans (i, s)))
          (lower, startState)

forExample :: String
forExample = for (5, 13)
                 (\(i, s) -> s ++ show i ++ " ")
                 ""

forExample' = concat $ intersperse " " $ map show [5..12]

forExample'' = intercalate " " $ map show [5..12]

-- show laziness
forExample''' = intercalate " " $ map show $ drop 4 $ take 12 $ [1..]



-- mapReduce
-- want to map values, and then aggregate
-- Aggregation should always remind you of Monoid
mapReduce :: Monoid m => (a -> m) -> [a] -> m
mapReduce f = mconcat . map f
