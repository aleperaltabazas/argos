
module Language.Argos.Parser
  ( argosParser
  , parseArgos
  , parseArgosFile
  )
where

import Data.Argos
import Language.Argos.Parser.Internal
import Text.Parsec
import Text.Parsec.String (Parser)

parseArgos :: String -> Either ParseError [Argument]
parseArgos = parse argosParser "argos"

parseArgosFile :: FilePath -> IO (Either ParseError [Argument])
parseArgosFile = (parseArgos <$>) . readFile

argosParser :: Parser [Argument]
argosParser = many1 (commandParser <|> optionParser)
