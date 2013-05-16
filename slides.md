
# Substitution

Lets's evaluate `mySum` on a small list, by following substitution rules

~~~~ {.haskell}
-- mySum []     = 0
-- mySum (x:xs) = x + (mySum xs)
mySum [1,2,3]
mySum 1:[2,3]
1 + (mySum [2,3])
1 + (mySum 2:[3])
1 + ( 2 + (mySum [3]))
1 + ( 2 + (mySum 3:[]))
1 + ( 2 + ( 3 + (mySum [])))
1 + ( 2 + ( 3 + (0)))
1 + ( 2 + ( 3 ))
1 + ( 5 )
6
~~~~

Inefficient: requires O(n) space!

# Tail Recursion

Can we use O(1) space? Let's use tail recursion to implement a new sum function:

~~~~ {.haskell}
-- Takes an accumulator as first argument
mySum' :: Int -> [Int] -> Int
mySum' acc []     = acc
mySum' acc (x:xs) = mySum' (acc+x) xs
-- ^ add the head of the list to the accumulator
~~~~

# Tail Recursion

To see why this takes constant space, let's use substitution rules:

~~~~ {.haskell}
-- mySum' acc []     = acc
-- mySum' acc (x:xs) = mySum' (acc+x) xs
mySum' 0     [1,2,3]
mySum' 0     1:[2,3]
mySum' (0+1) [2,3]
mySum' 1     2:[3]
mySum' (1+2) [3]
mySum' 3     3:[]
mySum' (3+3) []
mySum' 6     []
6
~~~~

This is equivalent to a flat loop in an imperative language
