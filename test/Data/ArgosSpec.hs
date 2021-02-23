module Data.ArgosSpec
  ( spec
  )
where

import Data.Argos
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = describe "ArgosSpec" $ describe "spread" $ do
  it "returns a map of length one" $ do
    spread Command { name = "foo", arguments = [] } `shouldBe` Map.fromList [(0, ["foo"])]
    spread Command { name = "bar", arguments = [] } `shouldBe` Map.fromList [(0, ["bar"])]
  it "returns a map of length two" $ do
    spread Command { name = "foo", arguments = [Option "bar" Nothing] } `shouldBe` Map.fromList [(0, ["foo"]), (1, ["bar"])]
    spread Command { name = "bar", arguments = [Command "foo" []] } `shouldBe` Map.fromList [(0, ["bar"]), (1, ["foo"])]
    spread Command { name = "foo", arguments = [Option "bar" Nothing, Option "baz" Nothing] }
      `shouldBe` Map.fromList [(0, ["foo"]), (1, ["bar", "baz"])]
