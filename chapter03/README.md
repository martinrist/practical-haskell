# Chapter 3 - Increasing Code Reuse

## Parametric Polymorphism

- Note the type of the `head` function:

    ```haskell
    > :t head
    head :: [a] -> a
    ```

- The `a` here is a _type variable_ that can be bound to different types
  depending on use:

    ```haskell
    > :set -XTypeApplications
    > :t head @Int
    head @Int :: [Int] -> Int

    > :t head "Hello"
    head "Hello" :: Char
    ```

- Functions like `head` are said to work for any value of the type parameter
  `a`.  This is referred to as _parametric polymorphism_ (c.f. C++ _templates_
  and Java _generics_).

- Polymorphism is available in both functions and data types, e.g.:

    ```haskell
    data Triple a b c = Triple a b c

    > :t Triple 42 "foo" True
    Triple 42 "foo" True :: Num a => Triple a [Char] Bool
    ```


## Functions as Parameters

### Higher-order Functions

- Consider using the function `map` to add 1 to all members of the list `[1, 2,
  3]`:

    ```haskell
    > succ 1
    2
    > map succ [1, 2, 3]
    [2,3,4]

    > :t map
    map :: (a -> b) -> [a] -> [b]
    ```

- The first parameter to `map` is itself a function, of type `a -> b`.
  Functions like this, that take other functions as parameters, are called
  _higher-order functions_.

- A commonly-used higher-order function is `$` (function application):

    ```haskell
    ($) :: (a -> b) -> a -> b
    f $ a = f a
    ```

- `$` is useful because it has very low operator precedence.  Therefore both
  sides of `$` will be evaluated before `f` is applied to `a`:

    ```haskell
    > maximum (map succ [1, 2, 3])
    4

    -- is the same as
    > maximum $ map succ [1, 2, 3]
    4
    ```


### Anonymous Functions

- _Anonymous functions_ have the form `\param1 param2 ... -> body`:

    ```haskell
    > map (\x -> x + 2) [1, 2, 3]
    [3,4,5]
    ```

- Certain types of pattern matching can occur on paramteres in anonymous
  functions just like other functions:

    ```haskell
    equalTuples :: [(Integer, Integer)] -> Bool
    equalTuples t = map (\(x, y) -> x == y) t
    ```

- By enabling the `LambdaCase` extension, we can use the special `\case` syntax
  to create an anonymous function with only one parameter to match on:

    ```haskell
    sayHello names = map (\case "Martin" -> "Hello there me!"
                                name     -> "Welcome, " ++ name
                         ) names
    ```


### Partial Application

- Consider a function to doulbe all elements in a list:

    ```haskell
    double list = map (\x -> x * 2) list

    -- or
    double = \list -> map (\x -> x * 2) list
    ```

- This can also be written as `double = map (\x -> x * 2)`, using _partial
  application_.

- We can also write `\x -> x * 2` using a _section_ as `(*2)`, so `double = map
  (*2)`.

- Partial application encourages a _point-free_ style of programming, where
  functions are combined without mentioning parameters.  Key to this is the
  _function composition_ operator `(.)`:

    ```haskell
    (.) :: (b -> c) -> (a -> b) -> a -> c
    f . g = \x -> f (g x)
    ```

- Using `.`, we can often rewrite functions, e.g."

    ```haskell
    doubleOdds list = map (*2) $ filter odd list

    -- can be written in point-free style as
    doubleOdds = map (*2) . filter odd
    ```

- The next two _combinators_ (functions that combine other functions) are used
  to convert multi-argument functions to single-argument functions that take
  tuples:

    ```haskell
    uncurry :: (a -> b -> c) -> (a, b) -> c
    uncurry f = \(x, y) -> f x y

    curry :: ((a, b) -> c) -> a -> b -> c
    curry f = \x y -> f (x, y)

    -- This is the _curried_ version of `max`
    > max 3 2
    3

    -- and the uncurried version
    > (uncurry max) (3, 2)
    3
    ```

- The `flip` combinator reverses the order of parameters in a function:

    ```haskell
    flip :: (a -> b -> c) -> (b -> a -> c)
    flip f = \x y -> f y z
    ```


## More about Modules

### Module imports

- Module imports allow definitions from one module to be used in another.

- The most basic way to import a module brings into scope all the definitions
  and makes them available for use as if they were declared in the importing
  module:

    ```haskell
    module PracticalHaskell.Chapter03.MoreModules where

    import Data.List

    permutationsStartingWith :: Char -> String -> [String]
    permutationsStartingWith letter
        = filter (\l -> head l == letter) . permutations
    ```

- To prevent name clashes, we can selectively import only certain definitions:

    ```haskell
    module PracticalHaskell.Chapter03.MoreModules where

    import Data.List (permutations, subsequence)

    -- remaining definitions unchanged
    ```

- Or we can import everything except specified definitions:

    ```haskell
    module PracticalHaskell.Chapter03.MoreModules where

    import Data.List hiding (head, tail)

    -- remaining definitions unchanged
    ```

- ADTs have two pieces of information, the type and its constructors, so imports
  are specified using `Type(ListOfConstructors)`:

    ```haskell
    module PracticalHaskell.Chapter03.MoreModules where

    import DataTypes (Client())                   -- only type, no constructors
    import DataTypes (Client(GovOrg, Individual)) -- only specified constructors
    import DataTypes (Client(..))                 -- all constructors
    ```

- _Qualified imports_ require prefixing the function with its full module name
  when used:

    ```haskell
    module PracticalHaskell.Chapter03.MoreModules where

    import qualified Data.List (filter, permutations)

    permutationsStartingWith :: Char -> String -> [String]
    permutationsStartingWith letter
        = Data.List.filter (\l -> head l == letter) . Data.List.permutations
    ```

- With qualified imports, the package name can be shortened:

    ```haskell
    module PracticalHaskell.Chapter03.MoreModules where

    import qualified Data.List as L

    permutationsStartingWith :: Char -> String -> [String]
    permutationsStartingWith letter
        = L.filter (\l -> head l == letter) . L.permutations
    ```


### Smart Constructors & Views

- As well as controlling which othe rmodules to _import_, it's possible to
  control which declarations in the current module are _exported_, by adding the
  export list as part of the `module` declaration:

    ```haskell
    module M (f) where

    f = ...
    g = ...
    ```

- With data types, there are several options for exporting, as for importing:
    - Just the type, not the constructor - thereby disallowing creation of
      values using the constructor.
    - The type and some subset of constructors.
    - The type and all its constructors.

- When discussing the [_default values
  idiom_](../chapter02/README.md#the-default-values-idiom) previously, there was
  no way to restrict the creation of `ConnOptions` values using the constructor.
  Now we can export only the datatype:

    ```haskell
    module PracticalHaskell.Chapter02.Records (ConnOptinos(), connDefault) where

    -- Remaining declarations unchanged
    ```

- This introduces the idea of _smart constructors_.  Consider a `Range` type
  representing a closed integer range between `a` and `b`, with the invariant `a
  <= b`:

    ```haskell
    data Range = Range Integer Integer deriving Show
    ```

- This won't prevent us creating invalid ranges:

    ```haskell
    > Range 10 5
    Range 10 5
    ```

- As an alternative, we create a _smart constructor_ function `range` that
  performs the check:

    ```haskell
    range :: Integer -> Integer -> Range
    range a b = if a <= b
                   then Range a b
                   else error "a must be <= b"

    > range 5 10
    Range 5 10

    > range 10 5
    *** Exception: a must be <= b
    ```

- Then we just export the `Range` type without constructors, and the smart
  constructor:

    ```haskell
    module PracticalHaskell.Chapter03.Ranges (Range(), range) where

    -- Remaining definitions
    ```

- However, code in other modules that attempts to pattern match on the `Range`
  constructor won't compile:

    ```haskell
    module PracticalHaskell.Chapter03.RangesClient where

    import PracticalHaskell.Chapter03.Ranges

    -- This function won't compile because `Range` is inaccessible
    -- prettyRange :: Range -> String
    -- prettyRange (Range a b) = "[" ++ show a ++ ", " ++ show b ++ "]"
    ```

- The solution is to create a new data type encoding the observed values of that
  type and use views when pattern matching:

    ```haskell
    module PracticalHaskell.Chapter03.Ranges
        (Range(), range, RangeObs(..), r) where

    -- Other definitions for `Range` and `range` unchanged

    data RangeObs = R Integer Integer deriving Show

    -- This will be used in the view pattern
    r :: Range -> RangeObs
    r (Range a b) = R a b
    ```

    ```haskell
    {-# LANGUAGE ViewPatterns #-}

    module PracticalHaskell.Chapter03.RangesClient where

    import PracticalHaskell.Chapter03.Ranges

    prettyRange :: Range -> String
    prettyRange rng = case rng of
                           (r -> R a b) -> "[" ++ show a ++ ", " ++ show b ++ "]"
    ```

- We can use a _pattern synonym_ to encapsulate this packaging and unpackaging
  of `Range` values so that our consumer doesn't need to be aware of its
  implementation:

    ```haskell
    {-# LANGUAGE PatternSynonyms #-}

    module PracticalHaskell.Chapter03.RangesSynonyms
        (Range(), range, pattern R) where

    -- Other definitions for `Range` and `range` unchanged

    pattern R :: Integer -> Integer -> Range
    pattern R a b <- Range a b
      where R a b = range a b
    ```

    ```haskell
    module PracticalHaskell.Chapter03.RangesSynonymsClient where

    import PracticalHaskell.Chapter03.RangesSynonyms

    prettyRange :: Range -> String
    prettyRange (R a b) = "[" ++ show a ++ ", " ++ show b ++ "]"
    ```
