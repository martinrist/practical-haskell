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
