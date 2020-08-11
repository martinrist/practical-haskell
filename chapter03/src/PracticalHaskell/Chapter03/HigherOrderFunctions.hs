{-# LANGUAGE LambdaCase #-}

module PracticalHaskell.Chapter03.HigherOrderFunctions where

sayHello :: [String] -> [String]
sayHello names = map (\name -> case name of
                                    "Martin" -> "Hello, you"
                                    _        -> "Welcome, " ++ name
                     ) names

sayHello' :: [String] -> [String]
sayHello' names = map (\case "Martin" -> "Hello, you"
                             name     -> "Welcome, " ++ name
                     ) names


-- Exercise 3.2

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (== 1)

filterANumber :: Integer -> [Integer] -> [Integer]
filterANumber x = filter (== x)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter (\x -> not $ p x)

data Client
  = GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving (Show)

data Person = Person String String
  deriving (Show)

isGovOrg :: Client -> Bool
isGovOrg (GovOrg _) = True
isGovOrg _ = False

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter isGovOrg

filterGovOrgs' :: [Client] -> [Client]
filterGovOrgs' = filter (\case (GovOrg _) -> True
                               _          -> False)