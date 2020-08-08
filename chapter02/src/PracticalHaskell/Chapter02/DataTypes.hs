{-# LANGUAGE ViewPatterns #-}

module PracticalHaskell.Chapter02.DataTypes where

data Client'
  = GovOrg' String
  | Company' String Integer String String
  | Individual' String String Bool
  deriving (Show)

data Client
  = GovOrg String
  | Company String Integer Person String
  | Individual Person Bool
  deriving (Show)

data Person = Person String String Gender
  deriving (Show)

-- Exercise 2.4
data Gender
  = Male
  | Female
  | Unknown
  deriving (Show, Eq)

data TravelDirection
  = Past
  | Future
  | Both
  deriving (Show)

data Manufacturer = Manufacturer String Integer deriving (Show)

data TimeMachine
  = TimeMachine Manufacturer Integer String TravelDirection Float
  deriving (Show)

clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name id person resp -> name
  Individual person ads -> case person of
    Person fNm lNm gender -> fNm ++ " " ++ lNm

clientName' :: Client -> String
clientName' client = case client of
  GovOrg name -> name
  Company name id person resp -> name
  Individual (Person fNm lNm _) _ -> fNm ++ " " ++ lNm

companyName :: Client -> Maybe String
companyName client = case client of
  Company name _ _ _ -> Just name
  _ -> Nothing

-- Exercise 2.5

clientsByGender :: [Client] -> Gender -> Integer
clientsByGender [] _ = 0
clientsByGender (c : cs) g =
  let remainingClients = clientsByGender cs g
   in case c of
        GovOrg _ -> remainingClients
        Company _ _ _ _ -> remainingClients
        Individual (Person _ _ g') _ ->
          if g' == g
            then remainingClients + 1
            else remainingClients

applyDiscounts :: Integer -> [TimeMachine] -> [TimeMachine]
applyDiscounts discount = fmap (applyDiscount discount)

applyDiscount :: Integer -> TimeMachine -> TimeMachine
applyDiscount discount (TimeMachine mfr model name dir price) =
  let price' = price * fromInteger (100 - discount) / 100
   in TimeMachine mfr model name dir price'

-- Guards
binom :: Integer -> Integer -> Integer
binom _ 0 = 1
binom x y | x == y = 1
binom n k = (binom (n - 1) (k - 1)) + (binom (n - 1) k)

-- Exercise 2.6

acker :: Integer -> Integer -> Integer
acker 0 n
  | n > 0 = n + 1
acker m 0
  | m > 0 = acker (m -1) 1
acker m n
  | m > 0 && n > 0 = acker (m -1) (acker m (n -1))

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a, b) : abs) =
  let (as, bs) = unzip' abs
   in (a : as, b : bs)

-- View Patterns

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Martin Rist") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False
