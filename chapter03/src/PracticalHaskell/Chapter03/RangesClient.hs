{-# LANGUAGE ViewPatterns #-}

module PracticalHaskell.Chapter03.RangesClient where

import PracticalHaskell.Chapter03.Ranges

-- This function won't compile because `Range` is inaccessible
-- prettyRange :: Range -> String
-- prettyRange (Range a b) = "[" ++ show a ++ ", " ++ show b ++ "]"

prettyRange :: Range -> String
prettyRange rng = case rng of
                       (r -> R a b) -> "[" ++ show a ++ ", " ++ show b ++ "]"