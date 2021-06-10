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
      spread Option { long = "foo", short = Nothing } `shouldBe` Map.fromList [(0, [Layer Nothing "--foo"])]
      spread Option { long = "bar", short = Nothing } `shouldBe` Map.fromList [(0, [Layer Nothing "--bar"])]
    it "returns the long option with two dashes at the beginning and the short option with one" $ do
      spread Option { long = "foo", short = Just 'f' } `shouldBe` Map.fromList [(0, [Layer Nothing "--foo -f"])]
      spread Option { long = "bar", short = Just 'b' } `shouldBe` Map.fromList [(0, [Layer Nothing "--bar -b"])]
    it "returns a map of length one" $ do
      spread Command { name = "foo", arguments = [] } `shouldBe` Map.fromList [(0, [Layer Nothing "foo"])]
      spread Command { name = "bar", arguments = [] } `shouldBe` Map.fromList [(0, [Layer Nothing "bar"])]
    it "returns a map of length two" $ do
      spread Command { name = "foo", arguments = [Option "bar" Nothing] }
        `shouldBe` Map.fromList [(0, [Layer Nothing "foo"]), (1, [Layer (Just "foo") "--bar"])]
      spread Command { name = "bar", arguments = [Command "foo" []] }
        `shouldBe` Map.fromList [(0, [Layer Nothing "bar"]), (1, [Layer (Just "bar") "foo"])]
      spread Command { name = "foo", arguments = [Option "bar" Nothing, Option "baz" Nothing] }
        `shouldBe` Map.fromList [(0, [Layer Nothing "foo"]), (1, [Layer (Just "foo") "--bar --baz"])]
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
        [ (0, [Layer Nothing "foo"])
        , (1, [Layer (Just "foo") "bar grault"])
        , (2, [Layer (Just "bar") "baz --biz qux", Layer (Just "grault") "--garply"])
        , (3, [Layer (Just "qux") "--corge"])
        ]
  describe "merge" $ do
    it "is an empty map" $ merge Map.empty Map.empty `shouldBe` Map.empty
    it "mixes both maps with no key merging"
      $          merge
                   (Map.fromList [(0, [Layer Nothing "foo"]), (2, [Layer Nothing "baz"])])
                   (Map.fromList [(1, [Layer Nothing "bar"]), (3, [Layer Nothing "biz"])])
      `shouldBe` Map.fromList
                   [ (0, [Layer Nothing "foo"])
                   , (1, [Layer Nothing "bar"])
                   , (2, [Layer Nothing "baz"])
                   , (3, [Layer Nothing "biz"])
                   ]
    it "merges the values with the same key"
      $          merge
                   (Map.fromList [(0, [Layer Nothing "foo"]), (1, [Layer Nothing "bar"]), (2, [Layer Nothing "qux quux"])])
                   (Map.fromList [(1, [Layer Nothing "baz"]), (3, [Layer Nothing "corge"])])
      `shouldBe` Map.fromList
                   [ (0, [Layer Nothing "foo"])
                   , (1, [Layer Nothing "bar", Layer Nothing "baz"])
                   , (2, [Layer Nothing "qux quux"])
                   , (3, [Layer Nothing "corge"])
                   ]
