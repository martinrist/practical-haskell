module PracticalHaskell.Chapter04.Sets where

import PracticalHaskell.Chapter04.Clients
import qualified Data.Map as M
import qualified Data.Set as S

classifyClient :: Client a -> ClientKind
classifyClient GovOrg { } = GovOrgKind
classifyClient Company { } = CompanyKind
classifyClient Individual { } = IndividualKind

type ClientsByKind = M.Map ClientKind (S.Set (Client Integer))

-- This version traverses the list element by element, performs
-- the classification, decides which map item to modify and then
-- adds itself to the set
classifyClients1 :: [Client Integer] -> ClientsByKind
classifyClients1 = foldr acc M.empty where
    acc :: Client Integer -> ClientsByKind -> ClientsByKind
    acc c ckm = M.insertWith S.union (classifyClient c) (S.singleton c) ckm where

-- This version creates list corresponding to the three kinds
-- of client, then converts those lists to sets and generates
-- the map from them
classifyClients2 :: [Client Integer] -> ClientsByKind
classifyClients2 cs = let govOrgs = filter (\c -> classifyClient c == GovOrgKind) cs
                          companies = filter (\c -> classifyClient c == CompanyKind) cs
                          individuals = filter (\c -> classifyClient c == IndividualKind) cs in
                        M.fromList [(GovOrgKind, S.fromList govOrgs),
                                    (CompanyKind, S.fromList companies),
                                    (IndividualKind, S.fromList individuals)
                                   ]


testClients :: Integer -> [Client Integer]
testClients n = fmap testGovOrg [1..n] ++
                fmap testCompany [1..n] ++
                fmap testIndividual [1..n]


testGovOrg :: Integer -> Client Integer
testGovOrg n = GovOrg n ("Gov Org #" ++ show n)

testPerson :: Integer -> Person
testPerson n = Person ("First " ++ show n) ("Last " ++ show n)

testCompany :: Integer -> Client Integer
testCompany n = Company n ("Company " ++ show n) (testPerson n) ("Duty " ++ show n)

testIndividual :: Integer -> Client Integer
testIndividual n = Individual n (testPerson n)

totalClients :: ClientsByKind -> Int
totalClients = length . concatMap (S.toList . snd) . M.toList