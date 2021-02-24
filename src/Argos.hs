module Argos
  ( module Data.Argos
  , module Language.Argos.AutoComplete
  )
where

import Data.Argos (ArgosTree, Argos(..), Argument(..), spread)
import Language.Argos.AutoComplete (parseArgos, parseArgosFile, autocompleteScript)
