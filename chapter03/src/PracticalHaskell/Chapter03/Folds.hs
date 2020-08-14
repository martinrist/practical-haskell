module PracticalHaskell.Chapter03.Folds where

import PracticalHaskell.Chapter03.HigherOrderFunctions

-- Exercise 3.3 - Your First Folds

productPattern :: [Integer] -> Integer
productPattern [] = 1
productPattern (x:xs) = x * productPattern xs

productFold :: [Integer] -> Integer
productFold = foldr (*) 1

clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name id person resp -> name
  Individual person ads -> case person of
    Person fNm lNm -> fNm ++ " " ++ lNm

minimumClientPattern :: [Client] -> Maybe Client
minimumClientPattern [] = Nothing
minimumClientPattern (x:xs) = case minimumClientPattern xs of
                                   Nothing  -> Just x
                                   Just y
                                   -- I can combine guards with cases
                                     | length (clientName x) < length (clientName y) -> Just x
                                     | otherwise -> Just y

minimumClientFold :: [Client] -> Maybe Client
minimumClientFold = foldr acc Nothing
    where acc :: Client -> Maybe Client -> Maybe Client
          acc c1 Nothing = Just c1
          acc c1 (Just c2)
            | length (clientName c1) < length (clientName c2) = Just c1
            | otherwise = Just c2

allPattern :: [Bool] -> Bool
allPattern [] = True
allPattern (x:xs) = x && allPattern xs

allFold :: [Bool] -> Bool
allFold = foldr (&&) True