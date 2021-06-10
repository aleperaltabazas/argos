{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Argos (compile, complete, parseArgosFile)
import Data.List.Split
import Options.Applicative
import System.Environment
import System.Exit
import System.Directory

data ArgosCommand
  = Compile CompileOptions
  | Complete CompleteOptions

data CompileOptions
  = CompileOptions
  { source :: String
  , progName :: String
  } deriving (Show, Eq)

data CompleteOptions
  = CompleteOptions
  { progName :: String
  , options :: [String]
  } deriving (Show, Eq)

sourceParser :: Parser String
sourceParser = strOption (long "source" <> short 's' <> metavar "FILENAME" <> help "Source .argos file")

targetParser :: Parser String
targetParser = strOption (long "target" <> short 't' <> metavar "FILENAME" <> help "Destination .bash file")

progNameParser :: Parser String
progNameParser = strArgument (metavar "PROGRAM-NAME" <> help "Program name for which to generate autocomplete script")

compileOptionsParser = CompileOptions <$> sourceParser <*> progNameParser

compileCommandParser = makeInfo "Compile an argos file into its autocompletion script" $ Compile <$> compileOptionsParser

optionsParser =
  words <$> strOption (long "options" <> short 'o' <> metavar "OPTIONS" <> help "Options passed to the script to generate")

completeOptionsParser = CompleteOptions <$> progNameParser <*> ((tail <$> optionsParser) <|> pure [])

completeCommandPaser = makeInfo "Return complete options for a given program and options" $ Complete <$> completeOptionsParser

argosCommandParser = subparser (command "compile" compileCommandParser <> command "complete" completeCommandPaser)

main :: IO ()
main = do
  opts <- execParser $ info (argosCommandParser <**> helper) (fullDesc <> header "Argos - autocompletion script generator")
  case opts of
    Complete CompleteOptions {..} -> do
      completions <- complete progName options
      putStrLn $ unwords completions
    Compile CompileOptions {..} -> do
      putStrLn "Parsing file..."
      result <- parseArgosFile source
      case result of
        Left err -> do
          putStrLn $ "Failed to parse " ++ source
          print err
        Right a -> do
          compile progName a
          putStrLn "Done!"

makeInfo :: String -> Parser a -> ParserInfo a
makeInfo desc parser = info (parser <**> helper) (progDesc desc)

argosFile prog = do
  h <- getHomeDirectory
  return $ h ++ "/.config/argos/" ++ prog ++ ".argos"
