{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Data.Argos
  ( Argument(..)
  , Layer(..)
  , OptionArgument(..)
  , LayeredArguments
  , spread
  , merge
  , layered
  )
where

import Control.Monad (forM_, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (execState, modify, State, get)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Extra as Map
import Data.Maybe (fromMaybe, fromJust)

data OptionArgument
  = Files
  { regex :: Maybe String
  }
  | Directories
  deriving (Show, Eq, Read)

data Argument
  = Command
  { name :: String
  , arguments :: [Argument]
  }
  | Option
  { long :: String
  , short :: Maybe Char
  , argument :: Maybe OptionArgument
  } deriving (Show, Eq, Read)

data Layer
  = Layer
  { previous :: Maybe String
  , current :: String
  } deriving (Show, Eq)

type LayeredArguments = Map Int [Layer]

layered :: [Argument] -> LayeredArguments
layered = foldl merge Map.empty . map spread

spread :: Argument -> LayeredArguments
spread arg = simplify $ flip execState Map.empty $ go Nothing 0 arg
 where
  go previous depth opt@Option {..} = modify (Map.insertAppended depth (Layer previous (asOption opt)))
  go previous depth Command {..}    = do
    modify (Map.insertAppended depth (Layer previous name))
    forM_ arguments $ go (Just name) (depth + 1)
  asOption Option {..} = ("--" ++ long) ++ maybe "" (\s -> " -" ++ [s]) short

merge :: LayeredArguments -> LayeredArguments -> LayeredArguments
merge xs ys = flip execState xs $ forM_ (Map.toList ys) $ \(depth, values) -> modify $ Map.insertManyAppended depth values

simplify :: LayeredArguments -> LayeredArguments
simplify = Map.mapWithKey go
 where
  go :: Int -> [Layer] -> [Layer]
  go key argos = flip execState [] $ forM_ argos $ \arg -> do
    curr <- get
    if any (\a -> previous a == previous arg) curr
      then modify $ mapIf (\a -> previous a == previous arg) (\a -> a { current = current a ++ " " ++ current arg })
      else modify (++ [arg])


  mapIf cond f = map (\x -> if cond x then f x else x)
