# HASKELL

## First Language

<blockquote>
“The most disastrous thing that you can ever learn is your first programming language.”
― Alan Kay (pioneer in OOP)
</blockquote>

* First programming language shapes habits, limits thinking
    * Overuse of `new` in C++ coming from Java
    * Indexed `for` loops in Python coming from C
* Good to "grok" others to gain perspective

## Haskell

* Haskell is unlike any other language
* Language Design decisions oppose the status quo
    * But brilliantly so
* Many advantages gained as a result
    * Plus, it's fun to program again!

![](images/HaskellLogo.jpg)

## QuickSort

~~~~ {.haskell}
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) =
    (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
~~~~

<aside class="notes">

* High level
* Declarative
* Elegant

* No variable assignments
* Breaks into cases
</aside>

## Haskell Platform

Cross-platform distribution with trusted libraries

[![Download Haskell Platform](http://hackage.haskell.org/platform/icons/button-100.png)](http://hackage.haskell.org/platform)

## Hackage

Rich open source library repository

## Cabal

Package management

[![](http://hackage.haskell.org/images/Cabal-dark.png)](https://github.com/haskell/cabal/wiki/cabal-install)

## GHC

Glasgow Haskell Compiler

## GHCi

Interpreter

## Hoogle

Search yours and other's libraries by type signatures

[![](http://www.haskell.org/hoogle/res/hoogle.png)](http://www.haskell.org/hoogle/)

## School Of Haskell

In-depth tutorials with inline runable/editable code

[![School Of Haskell](https://www.gravatar.com/avatar/76a473697ce9ed86d1d1ef514abbd5b8?s=200)](https://www.fpcomplete.com/school)

## Other Cool Stuff

* [QuickCheck](http://en.wikipedia.org/wiki/QuickCheck)
    * Generates test cases
    * Provides *minimal* failing test case on failure
* [Software Transactional Memory](https://www.fpcomplete.com/school/beautiful-concurrency)
    * Composable concurrency abstraction

## Haskell

More formally...

<blockquote>
Haskell is a purely functional, statically typed programming language, with non-strict evaluation.
</blockquote>

## **WAT**

# The Language

## The Language

* Purely functional
    * Pure - no side effects!
    * Functional - functions are first-class values!
* Statically typed
    * Rich type system
    * Inference engine
    * Type polymorphism, Kinds, Kind Polymorphism
* Non-strict evaluation
    * A.k.a. lazy evaluation

## Pure

> Applying a function with the same parameters always returns the same value

## Pure

* Functions are functions in the _mathematical_ sense
* No side effects within functions
    * No mutation, assignment or implicit state
       * Everything is immutable
    * No IO
    * No global state

## Pure

* Functions are referentially transparent
    * Repeatable results
    * Great for testing
* Allows for pervasive sharing
    * If everything immutable, no worry about mutation
    * Concurrency/Parallelism is trivial

## Pure

* Some intuition: Excel Spreadsheet
    * User-specified cells are inputs
    * Formula cells are outputs
    * Cells are not updated as state by Excel
    * Can be easily parallelized

TODO: Excel picture

## Functional

> Functions are first-class values

## Functional

* Functions
    * Can be passed around
    * Can be defined inline (lambda functions)
* Higher-order functions
    * Functions that return new functions

## Functional

* Limited intuition in imperative languages:
    * `qsort` from C

~~~~ {.cpp}
void qsort(
    void *ptr,
    size_t count,
    size_t size,
    // function as a parameter
    int (*comp)(const void *, const void *)
);
~~~~

<aside class="notes">
    * Function pointers not a closure
    * Can't return a new function
</aside>

## Lazy

> Values evaluated only when needed

## Lazy

* Some intuition:
    * Short circuiting of boolean expressions

~~~~ {.cpp}
if (pointer != NULL && pointer->doesSomething())
{
    // ...
}
else if (number < 0 || number > 100)
{
    // ...
}
~~~~

## Haskell Philosophy

* "What" rather than "How"
* Data flow
* Purity
* Composition

# Syntax Intro

## Defining Values

~~~~ {.haskell}
-- define a value
x = 42
-- cannot reassign x = 7

-- functions are also values
addOneTo v = v + 1
~~~~

* `=` is Mathematical Equality
* *Not* assignment
* Both sides are *equivalent*
    * Left can be substituted by right
    * And vice versa

<aside class="notes">
* Mention cannot reassign - not a VARiable
* Functions are values just like integers
* Values are Lowercase!
* Equational reasoning
</aside>

## Type annotations

~~~~ {.haskell}
x :: Int
x = 5

y :: Double
y = 4.2

addOneTo :: Int -> Int
addOneTo v = v + 1
~~~~

* Everything has a type
* `::` allows us to annotate values with types
* Concrete types always _Capitalized_
* `a -> b` denotes a function from `a` to `b`

## Function application

~~~~ {.haskell}
six :: Int
six = addOneTo 5

main :: IO ()
main = print six
~~~~

## New Data types

* We have seen some types
* Let's see the `Bool` definition

~~~~ {.haskell}
data Bool = True | False
~~~~

<aside class="notes">
* `Bool` is a new typename
* `True` and `False` are possible values
</aside>

## Pattern Matching

* We can inspect structure of arguments in function definitions
    * Called Pattern Matching

~~~~ {.haskell}
-- Using built-in if
boolToString :: Bool -> String
boolToString b = if b then "Yes!" else "No!"

-- Using Pattern Matching
boolToString' :: Bool -> String
boolToString' True  = "Yes!"
boolToString' False = "No!"
~~~~

<aside class="notes">
* "Unpacking" the argument
</aside>

## Factorial Example

~~~~ {.haskell}
-- Pattern matching and recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
~~~~

<aside class="notes">
* Mention `data Int = `1 | 2 | 3 | 4 ...`
</aside>

## Lists

Syntax                      Meaning
-----------                 ------------------------
`[]`                        Empty List
`[1, 2, 3]`                 List
`1 : [2, 3]`                Prepend to a list
`[1, 2] ++ [3, 4]`          Concatenate two lists
`"Hello" :: [Char]`         Strings are lists of `Char`
`"He" ++ "llo"`             Concatenate two strings

## List operations

Pattern match on `:` construction.

~~~~ {.haskell}
-- Get first element
head :: [a] -> a
head (x:xs) = x
~~~~

~~~~ {.haskell}
-- Get list after first element
tail :: [a] -> [a]
tail []     = []
tail (x:xs) = xs
~~~~

<aside class="notes">
* Pattern match against list construction `:`
* Parametric polymorphism 
* We don't assume anything about list elements themselves
* Cover empty case
* mention `error "empty list"` only if asked
</aside>

## Reverse a List

Pattern matching is your friend

~~~~ {.haskell}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
~~~~

Reuse `head` and `tail`:

~~~~ {.haskell}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse list = myReverse (tail list) ++ [head list]
~~~~

<aside class="notes">
* Can make it more generic
</aside>

# Function Example

## Summing a list

~~~~ {.haskell}
mySum :: [Int] -> Int
mySum []     = 0
mySum (x:xs) = x + (mySum xs)
~~~~

## Substitution

Let's evaluate `mySum` on a small list, by following substitution rules

~~~~ {.haskell}
-- mySum []     = 0
-- mySum (x:xs) = x + (mySum xs)
  mySum [1,2,3]
= mySum 1:[2,3]
= 1 + (mySum [2,3])
= 1 + (mySum 2:[3])
= 1 + ( 2 + (mySum [3]))
= 1 + ( 2 + (mySum 3:[]))
= 1 + ( 2 + ( 3 + (mySum [])))
= 1 + ( 2 + ( 3 + (0)))
= 1 + ( 2 + ( 3 ))
= 1 + ( 5 )
= 6
~~~~

Inefficient: requires O(n) space!

## Tail Recursion

* Can we use O(1) space?
* Use tail recursion to implement a new sum function:

~~~~ {.haskell}
-- Takes an accumulator as first argument
mySum' :: Int -> [Int] -> Int
mySum' acc []     = acc
mySum' acc (x:xs) = mySum' (acc + x) xs
-- ^ add the head of the list to the accumulator
~~~~

* No computation to be done after recursive call
* Essentially _replace_ the current stack with recursive call

## Tail Recursion

To see why this takes constant space, let's use substitution rules:

~~~~ {.haskell}
-- mySum' acc []     = acc
-- mySum' acc (x:xs) = mySum' (acc + x) xs
  mySum' 0     [1,2,3]
= mySum' 0     1:[2,3]
= mySum' (0+1) [2,3]
= mySum' 1     2:[3]
= mySum' (1+2) [3]
= mySum' 3     3:[]
= mySum' (3+3) []
= mySum' 6     []
= 6
~~~~

This is equivalent to a flat loop in an imperative language.

## Tail Recursion

* Let's encapsulate this accumulator implementation
* Use `where` for scoped definitions

~~~~ {.haskell}
mySum :: [Int] -> Int
-- call implementation with 0 accumulator
mySum xs = mySum' 0   xs
     where mySum' acc []     = acc
           mySum' acc (x:xs) = mySum' (acc + x) xs
~~~~

## Towards Reusability

* The type signature of `mySum` is quite restrictive
* We may want to sum
    * `Float`s
    * `Double`s
    * `Complex` numbers
* Can we make it generic?

# Type Inference

## Type Inference

* The compiler can infer types
* We can neglect type signatures

~~~~ {.haskell}
-- Compiles without type signature!
mySum xs = mySum' 0   xs
     where mySum' acc []     = acc
           mySum' acc (x:xs) = mySum' (acc + x) xs
~~~~

* And ask the compiler what it thinks:

~~~~ {.haskell}
-- Query the type in interpreter
> :type mySum
mySum :: Num a => [a] -> a
~~~~

## Type Inference - How?

~~~~ {.haskell}
--  Says "Type parameter a has to be a Number"
mySum :: Num a => [a] -> a
mySum xs = mySum' 0   xs
     where mySum' acc []     = acc
           mySum' acc (x:xs) = mySum' (acc + x) xs
--                the clue to the compiler ^
~~~~

* The compiler looks at what we have used:
    * Elements combined with operator `+`
* The "interface" that exposes `+` is `Num`
* Therefore `mySum` can only be used on lists of `Num`s

## Type Inference

* Let's try on different types:

~~~~ {.haskell}
> mySum [1, 2, 3, 4, 5, 6, 7]
28 :: Int

> mySum [2.5, 3.5, 4.5, 5.5, 6.5, 7.5]
30.0 :: Double
~~~~

## Type Inference - Benefits

* Compiler infers most generic type
* Genericity for free
* Type annotations more useful to programmers than compiler

<aside class="notes">
* Mention UML
</aside>

# Control Structures

## Control Structures

* Are there really no control structures?
    * `if` ?
    * `while` ?
    * `for` ?
* We can construct our own!
    * Due to Haskell's laziness 

<aside class="notes">
* Imperative control structures awkward in functional setting
* Better alternatives exist
</aside>

# Abstracting loops

## Abstracting loops

* For loops are very low level
    * No inherent semantics
* Transforming a list - `map`
* Accumulating over a list - `fold`

## Map

~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]
~~~~

`map f xs` is the list obtained by applying `f` to each element of `xs`, i.e.,

~~~~ {.haskell}
> map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
~~~~

<aside class="notes">
* Similar to C's `qsort`
</aside>

## Map

<figure>
<object width="100%" data="diagrams/MapFunction.svg" type="image/svg+xml">
    <img src="diagrams/MapFunction.png" />
</object>
</figure>

## Map

~~~~ {.haskell}
> map (*2) [1, 2, 3, 4, 5]
[2, 4, 6, 8, 10]
~~~~

<figure>
<object width="100%" data="diagrams/MapTimesTwo.svg" type="image/svg+xml">
    <img src="diagrams/MapTimesTwo.png" />
</object>
</figure>

<aside class="notes">
* Mention `std::transform` equivalent in stl
</aside>


## Functions

TODO: functions

* Functions are first class values!
    * Can be passed around
    * Can be equaded
    * Can be created

## Functions - Passed Around

* Accumulating over a list is abstracted as a "fold"

~~~~ {.haskell}
foldl :: (a -> b -> a) -> a -> [b] -> a
--       ~~~~~~~~~~~~~    ^- starting value (accumulator)
--      binary operator
~~~~

> foldl, applied to a __binary operator__, a __starting value__ (typically the
> left-identity of the operator), and __a list__, reduces the list using the
> binary operator, from left to right.

TODO: diagram of foldl?

## Functions - Passed Around

* Accumulating over a list is abstracted as a "fold"

~~~~ {.haskell}
foldl :: (a -> b -> a) -> a -> [b] -> a
(+)   :: Num a => a -> a -> a

mySum xs = foldl (+) 0 xs
~~~~

## Functions - Created

~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]

doubleValues xs = map (\x -> x * 2) xs
--                    ~^~~~~~~~~~~~
--                   lambda function
~~~~

~~~~ {.haskell}
> doubleValues [1..5]
[2, 4, 6, 8, 10]
~~~~

## Functions - Equaded

* Functions are values
* Values can be defined in terms of other values
* Define one function by equading to another
    * This is called _pointfree_ notation

~~~~ {.haskell}
toUpper :: Char -> Char
map     :: (a -> b) -> [a] -> [b]

stringToUpper :: String -> String
stringToUpper s = map (toUpper) s

-- factor out s
stringToUpper' :: String -> String
stringToUpper' = map (toUpper)
~~~~

## Functions - Equaded

~~~~ {.haskell}
stringToUpper :: String -> String
stringToUpper = map (toUpper) 
~~~~

~~~~ {.haskell}
> stringToUpper "Hello World"
"HELLO WORLD"
~~~~

## More Reusability?

* Our `mySum` function works great for adding a list of numbers
* But more types can be aggregated:
    * e.g. `String`s
* Can we capture this behaviour in an "interface"?

# Typeclasses

## Typeclasses

* Typeclasses are the "interfaces" of Haskell
* Much more flexible and general

~~~~ {.haskell}
-- The Num typeclass we saw before
class Num a where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
~~~~

## Typeclasses

* Anything can be treated as a number as long as it defines the below functions

~~~~ {.haskell}
-- The Num typeclass we saw before
class Num a where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
~~~~

## Typeclasses - Instances

* A Data Type can adhere to a typeclass by specifying an `instance`
* Attach behaviour after the fact onto existing Data Type

~~~~ {.haskell}
instance Num Int where
    (+) = intSum  -- assuming intSum
    (*) = intMult --          intMult
    (-) = intSub  --          intSub are provided elsewhere
    negate x =  0 - x
    abs x = if x < 0
            then negate x
            else x
~~~~

<aside class="notes">
* Filling in the `a`
</aside>

## More Typeclasses

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

## Generic functions

* Typeclasses decouple behaviour and polymorphism from data.
* Wealth of reusable functions to be used

~~~~ {.haskell}
sort :: Ord a => [a] -> [a]
-- example:
sort "Missisauga" = "Maagiissssu"

group :: Eq a => [a] -> [[a]]
-- example:
group "Missisauga" = ["M","i","ss","i","ss","a","u","g","a"]
~~~~

## Typeclasses

* Haskell has various interesting typeclasses:
    * `Monoid`s, `Functor`s, `Applicative`s, `Monad`s
* Each one is a very precise definition of what it means to be composable


<aside class="notes">
* Comes from category theory in mathematics
* We can use it anyway
</aside>

# The Monoid

## The Monoid

* The Monoid is a simple typeclass
* It decribes how to append two values of a datatype together

<blockquote>
"In abstract algebra, a __monoid__ is an algebraic structure with a single __associative binary operation__ and an __identity element__."
― Wikipedia
</blockquote>

## The Monoid

<blockquote>
"In abstract algebra, a __monoid__ is an algebraic structure with a single __associative binary operation__ and an __identity element__."
― Wikipedia
</blockquote>

~~~~ {.haskell}
class Monoid a where
    mempty :: a             -- identity
    mappend :: a -> a -> a  -- binary operation

-- Rules:  (not checked, but assumed in usage)
-- identity
mappend a mempty = a      
-- associativity
mappend a (mappend b c) = mappend (mappend a b) c 
~~~~

<aside class="notes">
* Don't worry about math jargon
* Examples incoming
</aside>

## The Monoid

* You know lots of `Monoid`s!

~~~~ {.haskell}
instance Monoid String where
    mempty = ""            -- empty string is identity
    mappend a b = a ++ b   -- op is string concatenation

instance Monoid Int where
    mempty = 0
    mappend a b = a + b

instance Monoid (Set a) where
    mempty = empty
    mappend s1 s2 = s1 `union` s2
~~~~

<aside class="notes">
* Mention `Sum Int` and `Product Int` as monoids
</aside>

## `mconcat`

* With a `Monoid` instance we get `mconcat` for free

~~~~ {.haskell}
-- Accumulate a value from a list, using mappend
mconcat :: Monoid m => [m] -> m
~~~~

* Can use it to sum numbers or concatenate a list of strings!
* Union a list of Sets!
* Join ethernet packets!
* Combine Databases!

# Map Reduce

## Map Reduce

Let's apply what we have learnt so far and formulate map-reduce (Popularized by Google)

> __MapReduce__ is a programming model for processing large data sets with a parallel, distributed algorithm on a cluster.

It won't be parallel, but captures concept

## Map Reduce

Two Steps:

* _Map_ ― applying transformation to input data
* _Reduce_ ― aggregating transformed data

## Map Reduce with Lists

Alread have map. Need reduce.

~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]

reduce :: ??
~~~~

## Reduce

Reduce combines values.

* Need combining function
* Need identity element

~~~~ {.haskell}
reduce :: (b -> b -> b) -> b -> [b] -> b
reduce combine startingValue transformedData = ??
~~~~

## Reduce

This looks like properties of a `Monoid`

~~~~ {.haskell}
class Monoid a where
    mempty :: a             -- identity
    mappend :: a -> a -> a  -- binary operation

reduce :: Monoid b => [b] -> b
~~~~

## Mconcat

We already have `mconcat`!

~~~~ {.haskell}
-- Accumulate a value from a list, using mappend
mconcat :: Monoid m => [m] -> m

reduce :: Monoid b => [b] -> b
reduce = mconcat
~~~~

## Map Reduce

We can now define `mapReduce`

~~~~ {.haskell}
mapReduce :: Monoid m => (a -> m) -> [a] -> m
mapReduce f list = reduce $ map f list
~~~~

## Map Reduce

As a composition of functions ("pointfree")

~~~~ {.haskell}
mapReduce :: Monoid m => (a -> m) -> [a] -> m
mapReduce f = reduce . map f
~~~~

## MapReduce

Use on a simple example of counting characters

~~~~ {.haskell}
countCharsInWords :: [String] -> Sum Int
countCharsInWords = mapReduce (Sum . length)
~~~~
~~~~ {.haskell}
> countCharsInWords ["Hello", "MapReduce!"]
Sum {getSum = 15}
~~~~

# Functors

## Problem

* You have a function `length :: String -> Int`
* But you need to apply it to a list `[String]`
* What do you do?
    * Ideally want `someFunc :: [String] -> [Int]`

## Map

* We saw this before
* `map` a function over a list

~~~~ {.haskell}
> map length ["Hello", "World!"]
[5, 6] :: [Int]
~~~~

## Generalizing map

* `map` applies a function to every element in a list
* Or more interestingly...
* It creates a new function that works on lists

~~~~ {.haskell}
> :type map
map :: (a -> b) -> [a] -> [b]

> :type map length
map length :: [String] -> [Int]
~~~~

## Lifting functions

With `map` we _lift_ a function to work with lists

<figure>
<object width="100%" data="diagrams/LiftingLength.svg" type="image/svg+xml">
    <img src="diagrams/LiftingLength.png" />
</object>
</figure>

## Lifting functions

Generalizing to arbitrary types `a` and `b`

<figure>
<object width="100%" data="diagrams/Map.svg" type="image/svg+xml">
    <img src="diagrams/Map.png" />
</object>
</figure>

## Key insight

* Have two _vertical_ types of arrows:
    * object to object
        * wrap inside list with `[]`
    * function to function
        * using `map` to _lift_ other functions

> Question: Given an arbitrary type wrapper, can we come up with a unique _lifting_ function?

TODO: give another motivating example in the form of `Maybe`?

## Functors

<figure>
<object width="100%" data="diagrams/Functors.svg" type="image/svg+xml">
    <img src="diagrams/Functors.png" />
</object>
</figure>

## Functor Composition

<figure>
<object width="100%" data="diagrams/FunctorComposition.svg" type="image/svg+xml">
    <img src="diagrams/FunctorComposition.png" />
</object>
</figure>

## Maybe Functor

~~~~ {.haskell}
instance Functor Maybe where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)
~~~~

# Monads


## Monads

<figure>
<object width="100%" data="diagrams/Endofunctors.svg" type="image/svg+xml">
    <img src="diagrams/Endofunctors.png" />
</object>
</figure>

## Monads

<figure>
<object width="100%" data="diagrams/EndofunctorsCompositionQuestion.svg" type="image/svg+xml">
    <img src="diagrams/EndofunctorsCompositionQuestion.png" />
</object>
</figure>

## Maybe

~~~~ {.haskell}
instance Monad Maybe where
    -- (>>=) :: m a -> (a -> m b) -> m b
    Nothing >>= f = Nothing
    Just a  >>= f = f a

    -- return :: a -> m a
    return a = Just a
~~~~

## Lists

~~~~ {.haskell}
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f []     = []
concatMap f (x:xs) = f x ++ concatMap f xs
~~~~

## Lists

~~~~ {.haskell}
instance Monad [] where
    -- concatMap :: (a -> [b]) -> [a] -> [b]
    -- (>>=)     :: m a -> (a -> m b) -> m b
    (>>=) = flip concatMap

    -- return :: a -> m a
    return a = [a]
~~~~

# Applicative Functor

## Applicative

~~~~ {.haskell}
class Functor f => Applicative f where
    pure :: a -> f a
    <*> :: f (a -> b) -> f a -> f b
~~~~

## Fmap of Diadic Functions

What happens if we `fmap` a function that takes more than one argument?

~~~~ {.haskell}
(+) :: Num a => a -> a -> a
fmap :: Functor f => (a -> b) -> f a -> f b

fmap (+) [1, 2, 3] = ??
~~~~

## Partially applied results

~~~~ {.haskell}
fmap :: (a -> b) -> f a -> f b
-- what if b is (c -> d)
fmap :: (a -> (c -> d)) -> f a -> f (c -> d)

fmap (+) [1, 2, 3] :: [(Int -> Int)]
fmap (+) [1, 2, 3] == [(+ 1), (+ 2), (+ 3)]
~~~~

## Applicative For Lists

~~~~ {.haskell}
instance Applicative [] where
    -- pure :: a -> f a
    pure x = [x]

    -- <*> :: f (a -> b) -> f a -> f b
    []     <*> vals = []
    (f:fs) <*> vals = fmap f vals ++ fs <*> vals
~~~~

## Using Applicative

~~~~ {.haskell}
<*> :: Applicative f => f (a -> b)  -> f a -> f b
<*> ::                   [(a -> b)] -> [a] -> [b]
~~~~

~~~~ {.haskell}
> (fmap (+) [1, 2, 3]) <*> [10, 17, 42] 
[11,18,43,12,19,44,13,20,45]
~~~~

## Alias for fmap

~~~~ {.haskell}
<$> = fmap
~~~~

~~~~ {.haskell}
> (+) <$> [1, 2, 3] <*> [10, 42] 
[11,43,12,44,13,45]
~~~~

## Reduction

~~~~ {.haskell}
--  []     <*> vals = []
--  (f:fs) <*> vals = fmap f vals ++ fs <*> vals
                      (+) <$> [1, 2, 3] <*> [10, 42] 
=                 [(+ 1), (+ 2), (+ 3)] <*> [10, 42]
=                (+ 1) : [(+ 2), (+ 3)] <*> [10, 42]
= fmap (+ 1) [10, 42] ++ [(+ 2), (+ 3)] <*> [10, 42]
= [11, 43] ++            [(+ 2), (+ 3)] <*> [10, 42]
= [11, 43] ++ [12, 44] ++       [(+ 3)] <*> [10, 42]
= [11, 43, 12, 44] ++           [(+ 3)] <*> [10, 42]
= [11, 43, 12, 44] ++ [13, 45] ++    [] <*> [10, 42]
= [11, 43, 12, 44, 13, 45] ++        [] <*> [10, 42]
= [11, 43, 12, 44, 13, 45] ++        []
= [11, 43, 12, 44, 13, 45]
~~~~

## Summary

~~~~ {.haskell}
fmap       ::   (a -> b)   -> f a -> f b
<*>        :: f (a -> b)   -> f a -> f b
flip (>>=) ::   (a -> m b) -> m a -> m b
~~~~

# Scrap

## Outline of topics

Need some kind of goal

* Value assignments
* Add/subtract
* Types
* Lists
* Strings
* Functions

## Ideas for small programs

* converting bank hex files to single binary file

## WIP Benefits of Lazy Evaluation

* Allows heavy reuse of code
* Give example of filtering a list
* fold, map, filter

## Syntax Overview

* `=` -- Define a value
* `::` -- Type annotation
* `[1, 2, 3]` -- List
* `:` -- Prepend to a list
* `++` -- Concatenate two lists
* `a -> b` -- Function from `a` to `b`
* `where` -- scoped definition

## Syntax Overview

Syntax                      Meaning
-----------                 ------------------------
`x = 42`                    Define a value
`42 :: Int`                 Type annotation
`a -> b`                    Function from `a` to `b`
`[1, 2, 3]`                 List
`1 : [2, 3]`                Prepend to a list
`"He" ++ "llo"`             Concatenate two lists
`where`                     scoped definition
