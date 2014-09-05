module Utilities where

import qualified Data.Map as Map
import qualified Data.Set as Set

listToMultimap :: (Ord a, Ord b) => [(a, b)] -> Map.Map a (Set.Set b)
listToMultimap entries = foldr inserter Map.empty entries
  where
--    inserter :: (a, b) -> Map.Map a (Set.Set b) -> Map.Map a (Set.Set b)
    inserter (k, v) m = insertWithM fn k v m

--    fn :: b -> Maybe (Set.Set b) -> Set.Set b
    fn v Nothing  = Set.singleton v
    fn v (Just s) = Set.insert v s

insertWithM :: Ord k => (b -> Maybe a -> a) -> k -> b -> Map.Map k a -> Map.Map k a
insertWithM fn key val m = Map.insert key (fn val $ Map.lookup key m) m