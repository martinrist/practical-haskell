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


### Sets

- Sets are found in the `Data.Set` module, which contains functions very similar
  to those in `Data.Map`, but without the `k` parameter.

- Sets enforce uniqueness of value:

    ```haskell
    > S.insert 4 $ S.singleton 4
    fromList [4]
    ```

- The `containers` library also provides `IntMap` and `IntSet`, which are
  higher-performance versions of `Map` and `Set` that use integer keys / elements.

- The `unordered-containers` library also provides `HashMap` and `HashSet`
  variants where the keys / elements are hashable to an `Int`.


### Trees

- _Trees_ are composed of _nodes_ which hold a value and have other trees as
  children.  They are found in the `Data.Tree` module, defined as:

    ```haskell
    data Tree   a = Node { rootLabel :: a, subForest :: Forest a }
    type Forest a = [Tree a]
    ```

- Other algorithms may require the use of other more specialised types of trees,
  and there are other packages for those.

- There are various ways to traverse a tree structure:
    - _Depth first, pre-order_ - each node recursively visits subtrees.  Node is
      visited _before_ subtrees.
    - _Depth first, post-order_ - as above, but node is visited _after_
      subtrees.
    - _Breadth-first_ - all nodes at a given level are visited in order before
      going down to the next level.

- Example of how to do pre-order traversal:

    ```haskell
    preOrder :: (a -> b) -> Tree a -> [b]
    preOrder f (Node v subtrees)
        = let subtreesTraversed = concat $ map (preOrder f) subtrees
              in f v : subtreesTraversed
    ```

- Pre-order traversal can be done using `Data.Tree.flatten`, and breadth-first
  using `Data.Tree.levels`.

- `Tree`s support mapping and folding, but unlike `Map`s and `Set`s this is via
  `fmap` in `Prelude` and `foldr` in `Data.Foldable`.


### Graphs

- _Graphs_ are composed of sets of vertices, joined by edges.  In the
  implementation in `Data.Graph`, nodes are identified by an `Int` and edges are
  _directed_ and without weight.

- One way to create a graph is using `graphFromEdges`, which takes a list of
  triples `(value, key, [key])`, where the third part is a list of neighbours of
  the node with key `key`.  Gives you back:
    - A `Graph`.
    - A function `Vertex -> (node, key, [key])` which maps a vertex identifier
      to the node information.
    - A function `key -> Maybe Vertex` which maps keys to vertex identifiers.

- Another way is to use `buildG` which takes a tuple with the minimum and
  maximum identifiers (the _bounds_) amd a list of _edges_ (tuples from vertex
  to vertex).

- A _topological sort_ (`topSort`) of a graph gives a list of the vertices in
  the order that they need to be carried out to preserve the dependencies.

- Given a `Graph` and two vertices, we can find out whether there is a path from
  the first to the second, by using `path`.  You can also determine the other
  vertices that can be reached from a given vertex using `reachable`.

- `Data.Graph` contains other functions that implement common graph algorithms,
  e.g.:
    - `scc` - strongly-connected components.
    - `components` - connected components.
