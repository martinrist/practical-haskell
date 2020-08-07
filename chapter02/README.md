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


## Working with Data Types


