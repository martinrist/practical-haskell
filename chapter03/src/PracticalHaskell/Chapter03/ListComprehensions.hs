{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}
module PracticalHaskell.Chapter03.ListComprehensions where

import PracticalHaskell.Chapter03.Clients
import GHC.Exts

companyAnalytics :: [Client a] -> [(String, [(Person, String)])]
companyAnalytics clients = [ (the clientName, zip person duty)
                           | client@(Company { .. }) <- clients
                           , then sortWith by duty
                           , then group by clientName using groupWith
                           , then sortWith by length client
                           ]

listOfClients
    = [ Individual 2 (Person "H. G." "Wells")
      , GovOrg 3 "NTTF" -- National Time Travel Foundation
      , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzschild") "Physicist"
      , Individual 5 (Person "Doctor" "")
      , Individual 6 (Person "Sarah" "Jane")
      ]