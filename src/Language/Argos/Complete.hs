{-# LANGUAGE RecordWildCards #-}

module Language.Argos.Complete where

import Control.Applicative
import Data.Argos
import Data.Maybe
import Data.List
import Language.Argos.Config

complete :: String -> [String] -> IO [String]
complete progName options = do
  acd       <- argosConfigurationDirectory
  arguments <- readArgos <$> readFile (acd ++ "/" ++ progName ++ ".data")
  return $ navigate options arguments
  where readArgos = read :: String -> [Argument]

navigate :: [String] -> [Argument] -> [String]
navigate []  args = concatMap completions args
navigate [o] args = fromMaybe fromIncomplete fromComplete
 where
  fromComplete   = fmap followUps . find (completed o) $ args
  fromIncomplete = concatMap completions . filter (canFollow o) $ args
navigate (o : os) args = fromMaybe [] $ do
  n <- find (completed o) args
  return $ case n of
    Command {..} -> navigate os arguments
    Option {..}  -> []

completed o Command {..} = o == name
completed o _            = False

canFollow o Command {..} = o `isPrefixOf` name
canFollow o Option {..}  = o `isPrefixOf` ("--" ++ long) || maybe False (\c -> o `isPrefixOf` ['-', c]) short

completions Command {..} = [name]
completions Option {..}  = ("--" ++ long) : maybe [] (\c -> ['-' : [c]]) short

followUps Command {..} = concatMap completions arguments
followUps _            = []
