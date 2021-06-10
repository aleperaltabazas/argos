{-# LANGUAGE QuasiQuotes #-}

module Language.Argos.Compile where

import Data.Argos (Argument)
import Data.String.Interpolate (i)
import Language.Argos.Config
import Text.Printf (printf)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)

compile :: String -> [Argument] -> IO ()
compile progName arguments = do
  acd <- argosConfigurationDirectory
  createDirectoryIfMissing True acd
  writeFile (acd ++ "/" ++ progName ++ ".data") $ show arguments
  let
    script =
      [i|#!/bin/bash

_#{progName}_completion()
{
    local cur len
    cur=${COMP_WORDS[COMP_CWORD]}
    ARGS="${COMP_WORDS[@]}"
    
    RES=$(argos complete #{progName} --options "$ARGS")
    COMPREPLY=($(compgen -W "$RES" -- "$cur"))
}

complete -F _#{progName}_completion #{progName}
|] :: String
  writeFile (progName ++ "-completion.bash") script
