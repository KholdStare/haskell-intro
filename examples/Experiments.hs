import Data.Char
import Data.List

-- do block works with a single monad

-- (>>=) :: m a -> (a -> m b) -> m b
main :: IO ()
main = getLine >>= putStrLn


main' :: IO ()
main' = do
    -- getLine :: IO String
    userLine <- getLine
    -- userLine :: String
    -- putStr :: String -> IO ()
    putStrLn userLine

-- getLine :: IO String
-- putStrLn :: String -> IO ()


alphabet :: [(Int, Char)]
alphabet = zip [1..] ['a'..'y']

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

maybeExample :: Int -> Maybe Char
maybeExample num = do
    letter <- lookup num alphabet
    return letter

maybeExample' :: Int -> Maybe Char
maybeExample' num = lookup num alphabet >>= return

-- toUpper :: Char -> Char
 

-- All the cool typeclasses
-- ========================
class MyFunctor f where
    myfmap :: (a -> b) -> f a -> f b

class MyFunctor f => MyApplicative f where
    myapply :: f (a -> b) -> f a -> f b
    mypure  :: a -> f a

class MyApplicative m => MyMonad m where
    mybind :: m a -> (a -> m b) -> m b
    myreturn :: a -> m a
    myreturn = mypure

-- 
instance MyFunctor Maybe where
    myfmap _ Nothing  = Nothing
    myfmap f (Just v) = Just $ f v

instance MyApplicative Maybe where
    myapply Nothing _ = Nothing
    myapply _ Nothing = Nothing
    myapply (Just f) (Just v) = Just $ f v

    mypure v = Just v

instance MyMonad Maybe where
    mybind Nothing _ = Nothing
    mybind (Just v) f = f v


-- Lists
-- =====

-- get first element of list
head' :: [a] -> a
head' (x:_) = x
-- non-exhaustive pattern match
-- -fwarn-incomplete-patterns

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

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
