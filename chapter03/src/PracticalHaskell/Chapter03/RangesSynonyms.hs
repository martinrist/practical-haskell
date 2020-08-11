{-# LANGUAGE PatternSynonyms #-}

module PracticalHaskell.Chapter03.RangesSynonyms
    (Range(), range, pattern R) where

data Range = Range Integer Integer deriving Show

range :: Integer -> Integer -> Range
range a b = if a <= b
               then Range a b
               else error "a must be <= b"

pattern R :: Integer -> Integer -> Range
pattern R a b <- Range a b
  where R a b = range a b