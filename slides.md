# Haskell - Why?

<blockquote>
“The most disastrous thing that you can ever learn is your first programming language.”
― Alan Kay (pioneer in OOP)
</blockquote>

* First programming language shapes habits, limits thinking
    * Overuse of `new` in C++ coming from Java
    * Indexed `for` loops in Python coming from C
* Obstructs idiomatic code in other languages
* What to do?

# Haskell - Why?

* Way to idiomatic code
    * Read books of experts
    * Read code of experts
    * Or...

TODO: Pictures books, github

# Haskell - Why?

* Try something radically different.
* Try something where nothing you know even applies.
* Try Haskell.

![](images/HaskellLogo.jpg)

# Haskell

<blockquote>
Haskell is a purely functional, statically typed programming language, with non-strict evaluation.
</blockquote>

WAT

# Haskell

* Purely functional
    * Pure - no side effects!
    * Functional - functions are first-class values!
* Statically typed
    * Rich type system
    * Inference engine
    * Type polymorphism, Kinds, Kind Polymorphism
* Non-strict evaluation
    * A.k.a. lazy evaluation

# Pure

* Functions are functions in the mathematical sense
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
    * Can be easily parallelized

# Functional

* Functions are first-class values
    * Can be passed around
    * Can be defined inline (lambda functions)
* Higher-order functions

# Functional

* Limited intuition in imperative languages:
    * `q_sort` from C

TODO: put code

# Lazy

* Lazy by default
* Evaluates values when they are needed
* Some intuition:
    * Short circuiting of boolean expressions

TODO: put code

# Haskell Philosophy

* "What" rather than "How"
* Data flow is king
* Keep things pure

# Intro

TODO: simple syntax breakdown

# Lists

TODO: lists

# Reverse

TODO: implement a reverse function?

# Summing a list

~~~~ {.haskell}
mySum :: [Int] -> Int
mySum []     = 0
mySum (x:xs) = x + (mySum xs)
~~~~

# Substitution

Let's evaluate `mySum` on a small list, by following substitution rules

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

# Tail Recursion

* Let's encapsulate this accumulator implementation
* Use `where` for scoped definitions

~~~~ {.haskell}
mySum :: [Int] -> Int
-- call implementation with 0 accumulator
mySum xs = mySum' 0   xs
     where mySum' acc []     = acc
           mySum' acc (x:xs) = mySum' (acc+x) xs
~~~~

# Reusability

* TODO: make generic for float etc.
* TODO: "ask" compiler

# Reusability

* Our `mySum` function works great for adding a list of numbers
* Can we make it more generic?
* What we want are `Monoid`s

# Typeclasses

* Typeclasses allow you to attach behaviour onto existing data

~~~~ {.haskell}
class Num a where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
~~~~

* Anything can be treated as a number as long as it defines the above functions

# Typeclasses

~~~~ {.haskell}
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

class Eq a => Ord a where
    -- minimal definition requires compare
    -- the rest are automatically defined in terms of it
    compare :: a -> a -> Ordering
    (<)  :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    (>)  :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    max  :: a -> a -> a
    min  :: a -> a -> a
~~~~

# Typeclasses

* Typeclasses decouple behaviour and polymorphism from data.
* Wealth of reusable functions to be used

~~~~ {.haskell}
sort :: Ord a => [a] -> [a]
sort "Missisauga" = "Maagiissssu"

group :: Eq a => [a] -> [[a]]
group "Missisauga" = ["M","i","ss","i","ss","a","u","g","a"]
~~~~

# Typeclasses

* Haskell has various interesting typeclasses:
    * Monoids, Functors, Applicatives, Monads
* Each one distills some essense of composability
TODO: doesn't mean anything ^

# The Monoid

* The Monoid is the simplest typeclass
* It decribes how to append two values of a datatype together

<blockquote>
"In abstract algebra, a __monoid__ is an algebraic structure with a single __associative binary operation__ and an __identity element__."
― Wikipedia
</blockquote>

~~~~ {.haskell}
class Monoid a where
    mempty :: a             -- identity
    mappend :: a -> a -> a  -- binary operation

-- Rules:  (not checked, but assumed in usage)
mappend a mempty = a      -- identity
mappend a (mappend b c) = mappend (mappend a b) c -- associativity
~~~~

# The Monoid

* You know lots of __Monoids__!

~~~~ {.haskell}
instance Monoid String where
    mempty = ""            -- empty string is identity
    mappend a b = a ++ b   -- binary op is string concatenation

instance Monoid Int where
    mempty = 0
    mappend a b = a + b

instance Monoid (Set a) where
    mempty = empty
    mappend s1 s2 = s1 `union` s2
~~~~

# `mconcat`

* With a `Monoid` instance we get `mconcat` for free

~~~~ {.haskell}
-- Accumulate a value from a list, using mappend
mconcat :: (Monoid m) => [m] -> m
~~~~

* Can use it to sum numbers or concatenate a list of strings!
* Union a list of Sets!
* Join ethernet packets!
* Combine Databases!

# Outline of topics

Need some kind of goal

* Value assignments
* Add/subtract
* Types
* Lists
* Strings
* Functions

# Ideas for small programs

* converting bank hex files to single binary file

# WIP Benefits of Lazy Evaluation

* Allows heavy reuse of code
* Give example of filtering a list
* fold, map, filter
