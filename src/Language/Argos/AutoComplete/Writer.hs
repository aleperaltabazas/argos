{-# LANGUAGE RecordWildCards #-}

module Language.Argos.AutoComplete.Writer
  ( writeNode
  )
where

import Data.Argos
import Data.Map (Map)
import Data.Map.Extra
import Data.List

writeNode :: Int -> [Argos] -> String
writeNode 0 argos = "    1)\n      COMPREPLY=($(compgen -W \"" ++ (unwords . map current $ argos) ++ "\" -- ${cur}))\n      ;;"
writeNode n argos =
  "    "
    ++ show (n + 1)
    ++ ")\n\
\      case ${prev} in\n"
    ++ (intercalate "\n" . map writeCase $ argos)
    ++ "\n\
\      esac\n\
\      ;;"
 where
  writeCase Argos { previous = Just prev, ..} =
    "        " ++ prev ++ ")\n\
\          COMPREPLY=($(compgen -W \"" ++ current ++ "\" -- ${cur}))\n\
\          ;;"
