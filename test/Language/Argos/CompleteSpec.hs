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
  removeFile "argos-test-completion.bash"
  removePathForcibly "argos-test"

setup = do
  (Right args) <- parseArgosFile "argos.argos"
  compile "argos-test" args

  createDirectory "argos-test"
  writeFile "argos-test/foo.argos" ""
  writeFile "argos-test/bar"       ""
  writeFile "argos-test/baz.argos" ""
  writeFile "argos-test/bar.argos" ""
  createDirectory "argos-test/qux"
  createDirectory "argos-test/quux"
  createDirectory "argos-test/corge"

spec :: Spec
spec = beforeAll_ setup $ afterAll_ cleanup $ describe "complete" $ do
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
  it "returns the 'foo' 'bar' and 'baz' when 'compile' and '-s' are passed" $ withCurrentDirectory "argos-test" $ do
    actual <- complete "argos-test" ["compile", "-s"]
    actual `shouldBe` ["foo.argos", "baz.argos", "bar.argos"]
  it "returns the 'bar' and 'baz' when 'compile', '-s' and 'b' are passed" $ withCurrentDirectory "argos-test" $ do
    actual <- complete "argos-test" ["compile", "-s", "b"]
    actual `shouldBe` ["baz.argos", "bar.argos"]
  it "returns an empty list when 'compile', '-s' and 'bar.argos' are passed" $ withCurrentDirectory "argos-test" $ do
    actual <- complete "argos-test" ["compile", "-s", "-b", "bar.argos"]
    actual `shouldBe` [" "]
