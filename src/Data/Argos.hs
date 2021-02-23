{-# LANGUAGE RecordWildCards #-}

module Data.Argos
  ( Argument(..)
  , Argos
  , spread
  , merge
  )
where

import Control.Monad (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Extra as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State (execState, modify, State)

data Argument
  = Command
  { name :: String
  , arguments :: [Argument]
  }
  | Option
  { long :: String
  , short :: Maybe Char
  } deriving (Show, Eq)

type Argos = Map Int [String]

spread :: Argument -> Argos
spread arg = flip execState Map.empty $ go 0 arg
 where
  go depth Option {..}  = modify (Map.insertManyAppended depth (("--" ++ long) : maybe [] (\s -> ['-' : [s]]) short))
  go depth Command {..} = do
    modify (Map.insertAppended depth name)
    forM_ arguments $ go (depth + 1)

merge :: Argos -> Argos -> Argos
merge xs ys = flip execState xs $ forM_ (Map.toList ys) $ \(depth, values) -> modify $ Map.insertManyAppended depth values
