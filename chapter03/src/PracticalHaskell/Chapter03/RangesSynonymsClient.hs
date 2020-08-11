module PracticalHaskell.Chapter03.RangesSynonymsClient where

import PracticalHaskell.Chapter03.RangesSynonyms

prettyRange :: Range -> String
prettyRange (R a b) = "[" ++ show a ++ ", " ++ show b ++ "]"