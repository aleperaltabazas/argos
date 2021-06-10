{-# LANGUAGE QuasiQuotes #-}

module Language.Argos.CompileSpec where

import Data.Argos
import Data.String.Interpolate (i)
import Language.Argos.Compile
import System.Directory
import Test.Hspec

cleanup = do
  h <- getHomeDirectory
  removeFile [i|#{h}/.config/argos/argos-test.data|]
  removeFile "argos-test-completion.bash"

spec :: Spec
spec = afterAll_ cleanup $ describe "compile" $ it "saves the show of the parsed arguments at ~/.config/argos" $ do
  let
    arguments =
      [ Command
        { name      = "compile"
        , arguments = [Option { long = "help", short = Just 'h' }, Option { long = "source", short = Just 's' }]
        }
      , Command
        { name      = "complete"
        , arguments = [Option { long = "help", short = Just 'h' }, Option { long = "options", short = Just 'o' }]
        }
      ]
  compile "argos-test" arguments
  h <- getHomeDirectory
  f <- readFile [i|#{h}/.config/argos/argos-test.data|]
  f `shouldBe` show arguments
