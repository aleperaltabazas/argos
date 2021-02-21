module Data.Argos where

data Argument
  = Command
  { name :: String
  , arguments :: [Argument]
  }
  | Option
  { long :: String
  , short :: Maybe Char
  } deriving (Show, Eq)
