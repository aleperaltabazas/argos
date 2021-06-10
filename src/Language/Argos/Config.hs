module Language.Argos.Config
  ( argosConfigurationDirectory
  )
where

import System.Directory (getHomeDirectory)

argosConfigurationDirectory = do
  h <- getHomeDirectory
  return $ h ++ "/.config/argos"
