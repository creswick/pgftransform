module Multimap where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Multimap k v = Map.Map k (Set.Set v)

get :: (Ord k, Ord v) => k -> Multimap k v -> Set.Set v
get k m = Map.findWithDefault Set.empty k m

put :: (Ord k, Ord v) => k -> v -> Multimap k v ->Multimap k v
put k v m = let set = get k m
            in Map.insert k (Set.insert v set) m

fromList :: (Ord k, Ord v) => [(k, v)] -> Multimap k v
fromList []      = Map.empty
fromList theList = foldr (\(k,v) acc -> put k v acc) Map.empty theList