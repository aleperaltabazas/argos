module Data.Map.Extra
  ( insertAppended
  , insertManyAppended
  )
where

import Data.Map (Map)
import qualified Data.Map as Map

insertAppended :: Ord k => k -> v -> Map k [v] -> Map k [v]
insertAppended k v = insertManyAppended k [v]

insertManyAppended :: Ord k => k -> [v] -> Map k [v] -> Map k [v]
insertManyAppended k vs = do
  currentVs <- Map.lookup k
  let newVs = maybe vs (++ vs) currentVs
  Map.insert k newVs
