module PracticalHaskell.Chapter04.TypeClasses where

import PracticalHaskell.Chapter04.Clients

class Nameable n where
    name :: n -> String

initial :: Nameable n => n -> Char
initial n = head (name n)

instance Nameable (Client i) where
    name Individual { person = Person { firstName = f, lastName = n } }
        = f ++ " " ++ n
    name c = clientName c
