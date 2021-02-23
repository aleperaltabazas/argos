module Data.Map.Extra
  ( insertAppended
  , insertManyAppended
  , mapToList
  )
where

import Data.Map (Map)
import qualified Data.Map as Map

insertAppended :: Ord k => k -> v -> Map k [v] -> Map k [v]
insertAppended k v = do
  vs <- Map.lookup k
  let newVs = maybe [v] (++ [v]) vs
  Map.insert k newVs

insertManyAppended :: Ord k => k -> [v] -> Map k [v] -> Map k [v]
insertManyAppended k vs = do
  currentVs <- Map.lookup k
  let newVs = maybe vs (++ vs) currentVs
  Map.insert k newVs

mapToList :: (k -> v -> u) -> Map k v -> [u]
mapToList f = map (uncurry f) . Map.toList
