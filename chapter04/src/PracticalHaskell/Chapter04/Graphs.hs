module PracticalHaskell.Chapter04.Graphs where

import Data.Graph

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph =
    [ ("wood", "wood", ["walls"])
    , ("plastic", "plastic", ["walls", "wheels"])
    , ("aluminium", "aluminium", ["wheels", "door"])
    , ("walls", "walls", ["done"])
    , ("wheels", "wheels", ["done"])
    , ("door", "door", ["done"])
    ]

timeMachineTravel :: Graph
timeMachineTravel = buildG (103, 2013)
    [(1302,1614),(1614,1302),(1302,2013),(2013,1302),(1614,2013)
    ,(2013,1408),(1408,1993),(1408,917),(1993,917),(917,103),(103,917)]