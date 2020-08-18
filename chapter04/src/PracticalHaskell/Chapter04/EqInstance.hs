module PracticalHaskell.Chapter04.EqInstance where

-- Exercise 4.5 - The Same Client
-- Note that we've redeclared Client / Person here without the derived instances

data Client i = GovOrg
                 { clientId   :: i
                 , clientName :: String
                 }
             | Company
                 { clientId   :: i
                 , clientName :: String
                 , person     :: Person
                 , duty       :: String
                 }
             | Individual
                 { clientId :: i
                 , person :: Person
                 }

data Person = Person
                 { firstName :: String
                 , lastName  :: String
                 }

instance Eq Person where
    Person { firstName = fn1, lastName = ln1 }
        == Person { firstName = fn2, lastName = ln2 }
            = fn1 == fn2 && ln1 == ln2

instance Eq i => Eq (Client i) where
    GovOrg { clientId = i1, clientName = n1 }
        == GovOrg { clientId = i2, clientName = n2 }
            = i1 == i2 && n1 == n2
    Company { clientId = i1, clientName = n1, person = p1, duty = d1 }
        == Company { clientId = i2, clientName = n2, person = p2, duty = d2 }
            = i1 == i2 && n1 == n2 && p1 == p2 && d1 == d2
    Individual { clientId = i1, person = p1 }
        == Individual { clientId = i2, person = p2 }
            = i1 == i2 && p1 == p2
    _ == _ = False