{-# LANGUAGE RecordWildCards #-}

module Main where

import Argos (parseArgosFile, autocompleteScript)
import Options.Applicative

newtype ArgosCommand
  = Compile CompileOptions

data CompileOptions
  = CompileOptions
  { source :: String
  , target :: String
  , progName :: String
  } deriving (Show, Eq)

sourceParser :: Parser String
sourceParser = strOption (long "source" <> short 's' <> metavar "FILENAME" <> help "Source .argos file")

targetParser :: Parser String
targetParser = strOption (long "target" <> short 't' <> metavar "FILENAME" <> help "Destination .bash file")

progNameParser :: Parser String
progNameParser = strArgument (metavar "PROGRAM-NAME" <> help "Program name for which to generate autocomplete script")

compileOptionsParser = CompileOptions <$> sourceParser <*> targetParser <*> progNameParser

compileCommandParser = makeInfo "Compile an argos file into its autocompletion script" $ Compile <$> compileOptionsParser

argosCommandParser = subparser (command "compile" compileCommandParser)

main :: IO ()
main = do
  opts <- execParser $ info (argosCommandParser <**> helper) (fullDesc <> header "Argos - autocompletion script generator")
  case opts of
    Compile CompileOptions {..} -> do
      result <- parseArgosFile source
      case result of
        Left err -> do
          putStrLn $ "Failed to parse " ++ source
          print err
        Right a -> do
          let script = autocompleteScript progName a
          writeFile target script

makeInfo :: String -> Parser a -> ParserInfo a
makeInfo desc parser = info (parser <**> helper) (progDesc desc)
