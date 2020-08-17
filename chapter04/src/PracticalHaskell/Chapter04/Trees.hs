module PracticalHaskell.Chapter04.Trees where

import Data.Tree

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees)
    = let subtreesTraversed = concat $ map (preOrder f) subtrees
          in f v : subtreesTraversed