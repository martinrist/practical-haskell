module PracticalHaskell.Chapter03.MoreModules where

import Data.List (permutations)

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter
    = filter (\l -> head l == letter) . permutations