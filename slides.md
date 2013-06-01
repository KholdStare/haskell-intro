# Haskell

<blockquote>
Haskell is a purely functional, statically typed programming langauge, with non-strict evaluation.
</blockquote>

WAT

# Haskell

* Purely functional
    * Pure - no side effects!
    * Functional - functions are first-class values!
* Statically typed
    * Rich type system
    * Inference engine
    * Type polymorphism, Kinds, Kind Polmorphism
* Non-strict evaluation
    * A.k.a. lazy evaluation

# Pure

* No side effects within functions
    * No mutation or implicit state - everything is immutable
    * No IO
    * No global state
* Functions are referentially transparent
    * Repeatable results
    * Great for testing
* Allows for pervasive sharing
    * If everything immutable, no worry about mutation
    * Concurrency/Parallelism is trivial

# Pure

* Some intuition: Excel Spreadsheet
    * User-specified cells are inputs
    * Formula cells are outputs
    * Cells are not updated as state by Excel
    * Can be made easily Parallel

# Functional

* Functions are first-class values
    * Can be passed around
    * Can be defined inline (lambda functions)
* Higher-order functions

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

* No computation to be done after recursive call
* Essentially replace the current context with recursive call

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

This is equivalent to a flat loop in an imperative language.

# WIP Benefits of Lazy Evaluation

* Allows heavy reuse of code
* Give example of filtering a list
