{-# LANGUAGE RecordWildCards #-}

module Language.Argos.Parser where

import Data.Argos
import Text.Parsec
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
whitespace = spaces <|> skipMany newline

nameParser = many1 (alphaNum <|> char '-' <|> char '_')

longParser :: Parser String
longParser = string "long" ~> char '(' ~> nameParser <~ char ')'

shortParser :: Parser Char
shortParser = string "short" ~> char '(' ~> letter <~ char ')'

optionParser :: Parser Argument
optionParser = do
  string "option" ~> char '('
  whitespace
  long <- longParser
  whitespace
  short <- optionMaybe (char ',' ~> shortParser)
  whitespace
  char ')'
  return Option { .. }

commandParser :: Parser Argument
commandParser = do
  string "command" ~> char '('
  whitespace
  name <- nameParser
  whitespace
  char ')'
  whitespace
  char '{'
  whitespace
  arguments <- sepBy (optionParser <|> commandParser) (char ',' <|> newline)
  return Command { .. }
