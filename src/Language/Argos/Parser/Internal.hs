{-# LANGUAGE RecordWildCards #-}

module Language.Argos.Parser.Internal where

import Data.Argos
import Data.Char
import Data.Maybe
import Text.Parsec
import Text.Parsec.Perm
import Text.Parsec.String (Parser)

(<<) :: Applicative f => f a -> f b -> f a
(<<) = (<*)

(<~) :: Parser a -> Parser b -> Parser a
a <~ b = do
  res <- a
  whitespace
  b
  whitespace
  return res

(~>) :: Parser a -> Parser b -> Parser b
a ~> b = do
  a
  whitespace
  res <- b
  whitespace
  return res

whitespace :: Parser ()
whitespace = spaces

nameParser = many1 (alphaNum <|> char '-' <|> char '_')

longParser :: Parser String
longParser = string "long" ~> char '(' ~> nameParser <~ char ')'

shortParser :: Parser Char
shortParser = string "short" ~> char '(' ~> letter <~ char ')'

optionArgumentParser :: Parser OptionArgument
optionArgumentParser = string "argument" ~> char '(' ~> op <~ char ')'
 where
  op :: Parser OptionArgument
  op = filesParser <|> directoriesParser

filesParser :: Parser OptionArgument
filesParser = Files <$> do
  whitespace
  caseInsensitive "files"
  whitespace
  r <- optionMaybe $ do
    char '('
    r <- many1 regexLike
    whitespace
    char ')'
    return r
  whitespace
  return r
  where regexLike = alphaNum <|> oneOf ".*-_[]"

directoriesParser :: Parser OptionArgument
directoriesParser = do
  whitespace
  caseInsensitive "directories"
  whitespace
  return Directories

optionParser :: Parser Argument
optionParser = do
  whitespace
  string "option" ~> char '('
  whitespace
  long <- longParser
  whitespace
  (short, argument) <- permute ((,) <$?> (Nothing, shortOpt) <|?> (Nothing, optArg))
  whitespace
  char ')'
  return Option { .. }
 where
  optArg = do
    whitespace
    a <- try $ optionMaybe (char ',' ~> optionArgumentParser)
    whitespace
    return a
  shortOpt = do
    whitespace
    s <- try $ optionMaybe (char ',' ~> shortParser)
    whitespace
    return s


commandParser :: Parser Argument
commandParser = do
  whitespace
  string "command" ~> char '('
  whitespace
  name <- nameParser
  whitespace
  char ')'
  whitespace
  arguments <- fromMaybe [] <$> optionMaybe nonEmptyCommandParser
  spaces
  return Command { .. }
 where
  nonEmptyCommandParser = do
    whitespace
    char '{'
    whitespace
    arguments <- (try commandParser <|> try optionParser) `sepBy` char ','
    whitespace
    char '}'
    whitespace
    return arguments

caseInsensitive :: String -> Parser String
caseInsensitive = mapM (\c -> char (toLower c) <|> char (toUpper c))
