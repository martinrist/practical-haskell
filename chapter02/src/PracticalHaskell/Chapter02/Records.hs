{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module PracticalHaskell.Chapter02.Records where

import           Data.Char (toUpper)

data ClientR = GovOrgR
                 { clientRName :: String
                 }
             | CompanyR
                 { clientRName :: String
                 , companyId   :: Integer
                 , person      :: PersonR
                 , duty        :: String
                 }
             | IndividualR
                 { person :: PersonR
                 }
  deriving (Show)

data PersonR = PersonR
                 { firstName :: String
                 , lastName  :: String
                 }
  deriving (Show)

-- Pattern Matching on Records

greet :: ClientR -> String
greet IndividualR {person = PersonR {firstName = fn}} = "Hi, " ++ fn
greet CompanyR {clientRName = c}                      = "Hi, " ++ c
greet GovOrgR {}                                      = "Welcome"

-- This version uses `NamedFieldPuns`
greet' :: ClientR -> String
greet' IndividualR {person = PersonR {firstName}} = "Hi, " ++ firstName
greet' CompanyR {clientRName}                     = "Hi, " ++ clientRName
greet' GovOrgR {}                                 = "Welcome"

-- This version uses `RecordWildCards`
greet'' :: ClientR -> String
greet'' IndividualR {person = PersonR {..}} = "Hi, " ++ firstName
greet'' CompanyR {..}                       = "Hi, " ++ clientRName
greet'' GovOrgR {}                          = "Welcome"

-- Updating Records

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR {firstName = initial : rest}) =
  let newName = (toUpper initial) : rest
   in p {firstName = newName}
nameInCapitals p@(PersonR {firstName = ""}) = p

-- Exercise 2.7 - Time Machine Records

data TravelDirection = Past | Future | Both deriving (Show)

data Manufacturer = Manufacturer
                      { mfrName      :: String
                      , foundingYear :: Integer
                      }
  deriving (Show)

data TimeMachine = TimeMachine
                     { manufacturer    :: Manufacturer
                     , model           :: Integer
                     , machineName     :: String
                     , travelDirection :: TravelDirection
                     , price           :: Float
                     }
  deriving (Show)

applyDiscounts :: Integer -> [TimeMachine] -> [TimeMachine]
applyDiscounts discount = fmap (applyDiscount discount)

applyDiscount :: Integer -> TimeMachine -> TimeMachine
applyDiscount discount m@(TimeMachine { price }) =
  let price' = price * fromInteger (100 - discount) / 100
   in m {price = price'}


-- Default Values Idiom

data ConnType = TCP | UDP

data UseProxy = NoProxy | Proxy String

data TimeOut = NoTimeOut | TimeOut Integer

data Connection = Connection String         -- Details elided

connect :: String -> ConnType -> Integer -> UseProxy
    -> Bool -> Bool
    -> TimeOut
    -> Connection
connect url connType speed proxy cache keepAlive timeOut = undefined
