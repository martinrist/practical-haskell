# Chapter 2 - Declaring the Data Model

## Characters, Numbers & Lists

- Character values (of type `Char`) contain one Unicode character.  Character
  literals are created using:
    - Character in single quotes - e.g. `'a'`
    - Code point in decimal - `\97` or hex `\x61`

- Many character-related functions exist in `Data.Char`:

    ```haskell
    > import Data.Char
    > :t toUpper
    toUpper :: Char -> Char

    > chr 97
    'a'

    > :t chr
    chr :: Int -> Char
    ```

- Haskell has various numeric types, e.g.:
    - `Int` - bounded integer type, containing values between at least +/-
      2^29-1.  Values of type `Int` usually have the native width of the
      architecture, which makes them fastest.
    - `Integer` - unbounded integral type without overflow / underflow.  Slower
      than `Int`.
    - `Ratio` - rational numbers of the form `n % m`.
    - `Float` / `Double` - floating-point types of single and double precision

- Converting between different numeric representations requires use of functions
  like `fromInteger` / `toInteger` and `fromRational` / `toRational`.

- Note that the types of numeric literals are _polymorphic_ - a literal can be
  used to create values of every type supporting the `Num` typeclass:

    ```haskell
    > :t 5
    5 :: Num a => a

    > :t 3.4
    3.4 :: Fractional a => a
    ```

- Strings are lists of characters - i.e. they have type `[Char]`.  However,
  string literals are written as expected - `"like this"`.

- List literals are written with comma-separated values:

    ```haskell
    > :t [1, 2, 3]
    [1, 2, 3] :: Num a => [a]

    > :t reverse
    reverse :: [a] -> [a]

    > reverse [1, 2, 3]
    [3, 2, 1]

    > reverse "abc"
    "cba"
    ```

- Lists are homogeneous, so all list elements must have the same type.

- Haskell lists are linked lists, with two basic construction operations:
    - `[]` - empty list
    - `(:)` - 'cons' - appends an element to an existing list:

    ```haskell
    > 1 : 2 : 3 : []
    [1, 2, 3]

    > 'a' : 'b' : 'c' : []
    "abc"
    ```

- Common functions in `Prelude` allow us to determine whether a list is empty
  (`null`) and to get the first (`head`) and rest (`tail`) of a list:

    ```haskell
    > null [1, 2, 3]
    False

    > head [1, 2, 3]
    1

    -- Can't take the head or tail of an empty list
    > head []
    *** Exception: Prelude.head: empty list
    > tail []
    *** Exception: Prelude.tail: empty list
    ```

## Creating a New Project

- Project can be created with Cabal using `cabal init`, or with Stack using
  `stack new`.

- `stack new` creates a project from a template, and creates various directories
  & project files, e.g.:
    - `stack.yaml` - Stack setup options, including the resolver, and other
      dependencies.
    - `project.cabal` - Cabal configuration file, consisting of a series of
      package properties followed by a number of _stanzas_ for various
      libraries, executables and test suites.
    - `package.yaml` - simpler-format file.  Stack uses `hpack` to generate the
      `.cabal` file.

- Haskell projects consist of _modules_.  Each module is contained in a
  corresponding file, and the filepath / filename corresponds to the
  hierarchical module name, e.g. the `PracticalHaskell.Chapter02.Example` is stored
  in `PracticalHaskell/Chapter02/Example.hs`.

- These files are relative to a source directory, which can be specified per
  stanza in the Cabal file (in `hs-source-dirs`) or in the `package.yaml` file
  (in `source-dirs`).

- Module files begin with module declarations:

    ```haskell
    module PracticalHaskell.Chapter02.Example where`
    ```

- To tell Cabal to compile a module file, include that module in a stanza, by
  adding it to either `exposed-modules` or `other-modules`.  Stack does this
  automatically.

- Stack is focused on having _reproducible builds_, and also manages your
  Haskell installation for you.  The `stack.yaml` file declares which version of
  the GHC compiler is targeted, via the resolver.

- Cabal by default uses _Hackage_ for dependencies - a community-maintained
  repository of packages.  Stack targets _Stackage_ by default, in which
  specific, mutually compatible versions of packages are grouped as _resolvers_.


## Defining Simple Functions

- Function body declarations have a _name_, a list of parameters, separated by
  spaces, and an `=` sign followed by the function body, e.g.:

    ```haskell
    firstOrEmpty lst = if not (null list) then head lst else "empty"
    ```

- Functions are usually preceded by type signatures, e.g.:

    ```haskell
    firstOrEmpty :: [[Char]] -> [Char]
    firstOrEmpty lst = if not (null list) then head lst else "empty"
    ```

- Here are some more examples of simple functions:

    ```haskell
    (+++) :: [a] -> [a] -> [a]
    lst1 +++ lst2 = if null lst1
                       then lst2
                       else (head lst1) : (tail lst1 ++ lst2)

    reverse2 :: [a] -> [a]
    reverse2 list = if null list
                       then []
                       else reverse2 (tail list) +++ [head list]
    ```

- To return more than one value from a function, use a _tuple_.  Note that the
  different components of a tuple can have different types, and that tuples with
  different numbers of components are different types.

- Haskell uses 'layout'-based syntax, in which all elements in the same block
  are supposed to start in the same column, e.g.:
    - In an `if` block, the lines for `then` and `else` must be indented the
      same way.
    - In a `let` or `where` block, all local bindings must start at the same
      level.


## Working with Data Types

### Algebraic Data Types

- The most basic kind of data type in Haskell is the _algebraic data type_
  (ADT), defined by two pieces of data:
    - A name for the type
    - A set of constructors to create new values.

    ```haskell
    data Client = GovOrg String
                | Company String Integer String String
                | Individual String String Bool

    > :t GovOrg "Nasa"
    GovOrg "Nasa" :: Client

    > :t Company "MyCorp" 123 "Mrs Smith" "CEO"
    Company "MyCorp" 123 "Mrs Smith" "CEO" :: Client
    ```

- To print results, we need an instance of the `Show` typeclass.  Can use
  Haskell's _automatic deriving_ feature to derive this:

    ```haskell
    data Client = GovOrg String
                | Company String Integer String String
                | Individual String String Bool
                deriving Show

    > Individual "Sandy" "Smith" False
    Individual "Sandy" "Smith" False
    ```


### Pattern Matching

- Extracting values from ADTs can be done using _pattern-matching_.  This allows
  matching against the constructor of the value, and creates bindings to the
  values encoded inside it.

- Simple patterns look like ADT constructors but with the paramters replaced by
  bindings:

    ```haskell
    clientName :: Client -> String
    clientName client = case client of
                            GovOrg name -> name
                            Company name id person resp -> name
                            Individual person ads ->
                                case person of
                                    Person fNm lNm gender -> fNm ++ " " ++ lNm
    ```

- We can combine the binding for the `Individual` case here:

    ```haskell
    clientName' :: Client -> String
    clientName' client = case client of
                              GovOrg name -> name
                              Company name id person resp -> name
                              Individual (Person fNm lNm _) _ -> fNm ++ " " ++ lNm
    ```

- Incomplete matches give non-exhaustive pattern warnings, and yield an
  exception at runtime if called.

- Functions like this that are not defined over the whole of the domain of their
  arguments are called _partial functions_ (as opposed to _total functions_).
  The `Maybe T` type is designed for cases such as this where a value may not be
  present:

    ```haskell
    companyName :: Client -> Maybe String
    companyName client = case client of
                            Company name _ _ _ -> Just name
                            _                  -> Nothing
    ```

- It's also possible to pattern match directly on `let` and `where` bindings,
  e.g.:

    ```haskell
    let Just name = companyName client
    ```

- Patterns are matched in the order in which they occur.  They do not backtrack
  when something goes wrong in the body of a match.

- When pattern-matching on the parameter to a function, the pattern can be
  encoded directly in the function definition, e.g.:

    ```haskell
    clientName :: Client -> String
    clientName (GovOrg name)                     = name
    clientName (Company name _ _ _)              = name
    clientName (Individual (Person fNm lNm _) _) = fNm ++ " " ++ lNm
    ```

- Pattern matching on lists can be done using the `:` constructor:

    ```haskell
    (+++) :: [a] -> [a] -> [a]
    list1 +++ list2 = case list1 of
                        []    -> list2
                        (x:xs) -> x:(xs +++ list2)

    -- or

    (+++) :: [a] -> [a] -> [a]
    []     +++ list2 = list2
    (x:xs) +++ list2 = x:(xs +++ list2)

    sorted :: [Integer] -> Bool
    sorted []       = True
    sorted [_]      = True
    sorted (x:y:zs) = x < y && sorted (y:zs)
    ```


### As Patterns

- An _as pattern_ allows you to bind some value in the match, while at the same
  time allowing matching on inner components, e.g.:

    ```haskell
    sorted :: [Integer] -> Bool
    sorted []           = True
    sorted [_]          = True
    sorted (x: r@(y:_)) = x < y && sorted r
    ```


### Guards

- _Guards_ allow a pattern to be refined using Boolean conditions that must be
  fulfilled by the bound values after a successful match, e.g.:

    ```haskell
    binom :: Integer -> Integer -> Integer
    binom _ 0 = 1
    binom x x = 1    -- This doesn't work - can only have one 'x'
    binom n k = ...

    -- Working version using guards
    binom _ 0          = 1
    binom x y | x == y = 1
    binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)
    ```

- Any expression returning a `Boolean` can be used as a guard condition,
  including `otherwise` as a catch-all clause (`otherwise == True`).


### View Patterns

- To look for patterns in a value but in some way that they're not encoded, we
  need to apply a function before checking the match.  This can be done with
  _view patterns_, enabled using the `ViewPatterns` language extension:

    ```haskell
    {-# LANGUAGE ViewPatterns #-}

    responsibility :: Client -> String
    responsibility (Company _ _ _ r) = r
    responsibility _ = "Unknown"

    specialClient :: Client -> Bool
    specialClient (clientName -> "Martin Rist") = True
    specialClient (responsibility -> "Director") = True
    specialClient _ = False
    ```

