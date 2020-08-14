module PracticalHaskell.Chapter03.Predicates where

import Data.List
import PracticalHaskell.Chapter03.Clients

-- Mini-exercise - `elem` using `find` and pattern matching

elemWithFind :: Eq a => a -> [a] -> Bool
elemWithFind x xs = case find (== x) xs of
                         Just _ -> True
                         Nothing -> False

-- Ordering

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual{person = p1}) (Individual{person = p2})
                                = compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2             = compare (clientName c1) (clientName c2)

listOfClients
    = [ Individual 2 (Person "H. G." "Wells")
      , GovOrg 3 "NTTF" -- National Time Travel Foundation
      , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
      , Individual 5 (Person "Doctor" "")
      , Individual 6 (Person "Sarah" "Jane")
      ]

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
                            sortBy (\x y -> compare (length y) (length x)) .
                            groupBy (\x y -> duty x == duty y) .
                            filter isCompany
                where isCompany (Company {}) = True
                      isCompany _            = False
