module Data.ArgosSpec
  ( spec
  )
where

import Data.Argos
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = describe "ArgosSpec" $ do
  describe "spread" $ do
    it "returns the long option with two dashes at the beginning" $ do
      spread Option { long = "foo", short = Nothing } `shouldBe` Map.fromList [(0, ["--foo"])]
      spread Option { long = "bar", short = Nothing } `shouldBe` Map.fromList [(0, ["--bar"])]
    it "returns the long option with two dashes at the beginning and the short option with one" $ do
      spread Option { long = "foo", short = Just 'f' } `shouldBe` Map.fromList [(0, ["--foo", "-f"])]
      spread Option { long = "bar", short = Just 'b' } `shouldBe` Map.fromList [(0, ["--bar", "-b"])]
    it "returns a map of length one" $ do
      spread Command { name = "foo", arguments = [] } `shouldBe` Map.fromList [(0, ["foo"])]
      spread Command { name = "bar", arguments = [] } `shouldBe` Map.fromList [(0, ["bar"])]
    it "returns a map of length two" $ do
      spread Command { name = "foo", arguments = [Option "bar" Nothing] } `shouldBe` Map.fromList [(0, ["foo"]), (1, ["--bar"])]
      spread Command { name = "bar", arguments = [Command "foo" []] } `shouldBe` Map.fromList [(0, ["bar"]), (1, ["foo"])]
      spread Command { name = "foo", arguments = [Option "bar" Nothing, Option "baz" Nothing] }
        `shouldBe` Map.fromList [(0, ["foo"]), (1, ["--bar", "--baz"])]
    it "returns a map of length four" $ do
      let
        c = Command
          { name      = "foo"
          , arguments =
            [ Command
              { name      = "bar"
              , arguments =
                [ Command { name = "baz", arguments = [] }
                , Option { long = "biz", short = Nothing }
                , Command { name = "qux", arguments = [Option { long = "corge", short = Nothing }] }
                ]
              }
            , Command { name = "grault", arguments = [Option { long = "garply", short = Nothing }] }
            ]
          }
      spread c `shouldBe` Map.fromList
        [(0, ["foo"]), (1, ["bar", "grault"]), (2, ["baz", "--biz", "qux", "--garply"]), (3, ["--corge"])]
  describe "merge" $ do
    it "is an empty map" $ merge Map.empty Map.empty `shouldBe` Map.empty
    it "mixes both maps with no key merging"
      $          merge (Map.fromList [(0, ["foo"]), (2, ["baz"])]) (Map.fromList [(1, ["bar"]), (3, ["biz"])])
      `shouldBe` Map.fromList [(0, ["foo"]), (1, ["bar"]), (2, ["baz"]), (3, ["biz"])]
    it "merges the values with the same key"
      $ merge (Map.fromList [(0, ["foo"]), (1, ["bar"]), (2, ["qux", "quux"])]) (Map.fromList [(1, ["baz"]), (3, ["corge"])])
      `shouldBe` Map.fromList [(0, ["foo"]), (1, ["bar", "baz"]), (2, ["qux", "quux"]), (3, ["corge"])]
