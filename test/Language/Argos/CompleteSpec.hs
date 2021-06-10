{-# LANGUAGE QuasiQuotes #-}

module Language.Argos.CompleteSpec where

import Data.String.Interpolate (i)
import Language.Argos.Compile
import Language.Argos.Complete
import Language.Argos.Parser
import System.Directory
import Test.Hspec

cleanup = do
  h <- getHomeDirectory
  removeFile [i|#{h}/.config/argos/argos-test.data|]

setup = do
  (Right args) <- parseArgosFile "argos.argos"
  compile "argos-test" args

spec :: Spec
spec = beforeAll setup $ afterAll_ cleanup $ describe "complete" $ do
  it "returns all of the root elements if no options are passed" $ do
    actual <- complete "argos-test" []
    actual `shouldBe` ["compile", "complete", "--help", "-h"]
  it "returns complete and compile when 'comp' is the only option passed" $ do
    actual <- complete "argos-test" ["comp"]
    actual `shouldBe` ["compile", "complete"]
  it "returns all of the complete arguments when 'complete' is the only option passed" $ do
    actual <- complete "argos-test" ["complete"]
    actual `shouldBe` ["--help", "-h", "--options", "-o"]
  it "returns the complete options when 'complete' and '-' are passed" $ do
    actual <- complete "argos-test" ["complete", "-"]
    actual `shouldBe` ["--help", "-h", "--options", "-o"]
