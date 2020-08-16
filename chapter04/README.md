# Chapter 4 - Using Containers & Type Classes

## Using Packages

- A _package_ is the distribution unit of code understood by Cabal and Stack:
    - Cabal uses [Hackage](http://hackage.haskell.org) as its package database.
    - Stack uses [Stackage](https://www.stackage.org), which provides snapshots
      of Hackage, called _resolvers_.

- To add a package dependency in Cabal, add the package name to the
  `build-depends` property for the appropriate stanza.

- Dependencies may specify constraints over the required version, e.g.
  `containers >= 0.6.0.1 && < 0.7`.

- The meaning of version numbers is defined by the Haskell [Package Versioning
  Policy](http://pvp.haskell.org).

- First step when building packages with Stack is to create a `stack.yaml` file,
  which can be done by running `stack init`:
    - After `stack init`, `stack setup` will download the required version of
      GHC if necessary.

- Package can be built with Stack using `stack build`.

- The `stack.yaml` file contains:
    - `resolver` - specifies the resolver to be used (e.g. `lts-16.9`)
    - `packages` - lists the user packages to be built - defaults to `.` but can
      be used to create project with multiple packages.
    - `extra-deps` - specifies extra dependencies - i.e. packages that are only
      available in Hackage.  These have both a package name and version - e.g.
      `wonderful-0.2.1.0`.


## Containers - Maps, Sets, Trees, Graphs

### Maps

- _Maps_ allow efficient association of values with unqiue keys.

- Map implementations can be found in the `Data.Map` module of the `containers`
  package.

- The basic map type is `Map k a`, where `k` is the type of keys, and `a` is the
  type of values, e.g.: `Map Client [Product]`

- Haskell has no special syntax for creating maps, but empty maps are created
  using `empty`, and maps with a single element using `singleton`:

    ```haskell
    > M.empty
    fromList []

    > M.singleton "hello" 3
    fromList [("hello",3)]
    ```

- Can create a `Map` from a list of key/value pairs using `fromList`:

    ```haskell
    -- Note how the last value corresponding to duplicate keys is retained
    > M.fromList [("hello", 1), ("bye", 2), ("hello", 3)]
    fromList [("bye", 2), ("hello", 3)]
    ```

- Can insert values using `insert` (replaces values with duplicate keys) or
  `insertWith` (provides a combining function):

    ```haskell
    > M.insert "bye" 3 $ M.singleton "hello" 2
    fromList [("bye",3),("hello",2)]

    > M.insert "hello" 5 $ M.singleton "hello" 2
    fromList [("hello",5)]

    > M.insertWith (+) "hello" 5 $ M.singleton "hello" 2
    fromList [("hello",7)]
    ```

- `null` can be used to check for an empty list, and `member` can be used to
  check for key membership:

    ```haskell
    > M.null M.empty
    True

    > M.null $ M.singleton "hello" 2
    False

    > M.member "hello" $ M.singleton "hello" 4
    True

    > M.member "hello" $ M.singleton "HELLO" 4
    False
    ```

- To get the value of a key, use `lookup`, which returns a `Maybe a`:

    ```haskell
     > M.lookup "hello" $ M.singleton "bye" 2
    Nothing
    > M.lookup "hello" $ M.singleton "hello" 4
    Just 4
    ```

- Alternatively, `findWithDefault` returns a default value if the specified key
  isn't present:

    ```haskell
    > M.findWithDefault 0 "hello" $ M.empty
    0

    > M.findWithDefault 0 "hello" $ M.singleton "hello" 1
    1
    ```

- Key / value pairs can be deleted using `delete`, specifying the key to be
  deleted.  This is a no-op if the key to be deleted doesn't exist in the map:

    ```haskell
    > M.delete "hello" $ M.singleton "hello" 2
    fromList []

    > M.delete "hello" $ M.singleton "bye" 4
    fromList [("bye",4)]
    ```

- Values in the map can be amended by applying a function to them using
  `adjust`:

    ```haskell
    >  M.adjust (+7) "hello" $ M.singleton "hello" 1
    fromList [("hello",8)]
    ```

- There's a more general funciton, `alter` that combines `insert`, `delete` and
  `adjust`.  It takes a function `Maybe a -> Maybe a`.  The input to this
  function will be `Nothing` / `Just oldValue`, and it will return `Nothing` for
  deletion, and `Just newValue` if not.

- Maps can also be combined using `union`, `intersection` and `difference`,
  which will apply the set operations to the set of keys.

- The functions `map`, `foldr`, `foldl` and `filter` have counterparts in
  `Data.Map` that apply similar functions as for lists.  There are `withKey`
  variants that pass the key along to the mapping / accumulator function:

    ```haskell
    > :t M.map
    M.map :: (a -> b) -> M.Map k a -> M.Map k b

    > :t M.mapWithKey
    M.mapWithKey :: (k -> a -> b) -> M.Map k a -> M.Map k b
    ```
