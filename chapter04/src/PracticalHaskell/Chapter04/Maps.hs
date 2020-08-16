module PracticalHaskell.Chapter04.Maps where

import qualified Data.Map as M
import Control.Applicative

-- Exercise 4.2 - Altering your maps
-- M.alter :: (Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k v = M.alter (const $ Just v) k

delete :: Ord k => k -> M.Map k a -> M.Map k a
delete k = M.alter (const Nothing) k

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f k = M.alter (liftA f) k
