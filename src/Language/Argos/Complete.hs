{-# LANGUAGE RecordWildCards #-}

module Language.Argos.Complete
  ( complete
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Argos
import Data.Maybe
import Data.List
import Language.Argos.Config
import System.Directory
import Text.Regex.Posix

complete :: String -> [String] -> IO [String]
complete progName options = do
  acd       <- argosConfigurationDirectory
  arguments <- readArgos <$> readFile (acd ++ "/" ++ progName ++ ".data")
  navigate options arguments
  where readArgos = read :: String -> [Argument]

navigate :: [String] -> [Argument] -> IO [String]
navigate []  args = return $ concatMap completions args
navigate [o] args = fromMaybe (return fromIncomplete) fromComplete
 where
  fromComplete = do
    c <- find (completed o) args
    return $ followUps c
  fromIncomplete = concatMap completions . filter (canFollow o) $ args
navigate [o, curr] args = if not $ "-" `isPrefixOf` o
  then deepenOrEmpty o [curr] args
  else fromMaybe (return [" "]) $ do
    o <- optArg
    return $ do
      fs <- case o of
        Directories -> listCurrentDirectory doesDirectoryExist curr
        Files {..}  -> do
          fs <- listCurrentDirectory doesFileExist curr
          case regex of
            Nothing -> return fs
            Just r  -> return $ filter (=~ r) fs
      return $ if curr `elem` fs then [" "] else fs
 where
  optArg = do
    Option {..} <- mfilter isOption . find (completed o) $ args
    argument
  isOption Option{} = True
  isOption _        = False

  fromOption = do
    o <- optArg
    return $ case o of
      Directories -> listCurrentDirectory doesDirectoryExist curr
      Files {..}  -> do
        fs <- listCurrentDirectory doesFileExist curr
        case regex of
          Nothing -> return fs
          Just r  -> return $ filter (=~ r) fs
navigate (o : os) args = deepenOrEmpty o os args

deepenOrEmpty o os args = do
  let n = find (completed o) args
  case n of
    Just Command {..} -> navigate os arguments
    _                 -> return [" "]

listCurrentDirectory filtering curr =
  filter (curr `isPrefixOf`) <$> (getCurrentDirectory >>= listDirectory >>= filterM filtering)

completed o Command {..} = o == name
completed o Option {..}  = "--" ++ long == o || ((\c -> ['-', c]) <$> short) == Just o

canFollow o Command {..} = o `isPrefixOf` name
canFollow o Option {..}  = o `isPrefixOf` ("--" ++ long) || maybe False (\c -> o `isPrefixOf` ['-', c]) short

completions Command {..} = [name]
completions Option {..}  = ("--" ++ long) : maybe [" "] (\c -> ['-' : [c]]) short

followUps Command {..} = return $ concatMap completions arguments
followUps Option {..}  = case argument of
  Just Files { regex = Just r } -> do
    fs <- listCurrentDirectory doesFileExist ""
    return $ filter (=~ r) fs
  Just Files { regex = Nothing } -> listCurrentDirectory doesFileExist ""
  Just Directories               -> listCurrentDirectory doesDirectoryExist ""
  _                              -> return [" "]
