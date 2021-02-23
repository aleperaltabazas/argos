{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Data.Argos
  ( Argument(..)
  , Argos(..)
  , ArgosTree
  , spread
  , merge
  )
where

import Control.Monad (forM_, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (execState, modify, State, get)
import Data.List.Extra (mapIf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Extra as Map
import Data.Maybe (fromMaybe)

data Argument
  = Command
  { name :: String
  , arguments :: [Argument]
  }
  | Option
  { long :: String
  , short :: Maybe Char
  } deriving (Show, Eq)

data Argos
  = Argos
  { previous :: Maybe String
  , current :: String
  } deriving (Show, Eq)

type ArgosTree = Map Int [Argos]

spread :: Argument -> ArgosTree
spread arg = simplify $ flip execState Map.empty $ go Nothing 0 arg
 where
  go previous depth opt@Option {..} = modify (Map.insertAppended depth (Argos previous (asOption opt)))
  go previous depth Command {..}    = do
    modify (Map.insertAppended depth (Argos previous name))
    forM_ arguments $ go (Just name) (depth + 1)
  asOption Option {..} = ("--" ++ long) ++ maybe "" (\s -> " -" ++ [s]) short

merge :: ArgosTree -> ArgosTree -> ArgosTree
merge xs ys = flip execState xs $ forM_ (Map.toList ys) $ \(depth, values) -> modify $ Map.insertManyAppended depth values

simplify :: ArgosTree -> ArgosTree
simplify = Map.mapWithKey go
 where
  go :: Int -> [Argos] -> [Argos]
  go key argos = flip execState [] $ forM_ argos $ \arg -> do
    curr <- get
    if any (\a -> previous a == previous arg) curr
      then modify $ mapIf (\a -> previous a == previous arg) (\a -> a { current = current a ++ " " ++ current arg })
      else modify (++ [arg])
