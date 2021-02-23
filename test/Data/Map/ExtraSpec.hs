module Data.Map.ExtraSpec
  ( spec
  )
where

import Data.Map.Extra
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = describe "Data.Map.Extra" $ describe "insertAppended" $ do
  let m = Map.fromList [(1, ["foo"]), (2, ["bar"]), (3, ["baz"])]
  it "creates a new entry in the map if the key does not exist" $ do
    insertAppended 4 "biz" m `shouldBe` Map.fromList [(1, ["foo"]), (2, ["bar"]), (3, ["baz"]), (4, ["biz"])]
    insertAppended 10 "biz" m `shouldBe` Map.fromList [(1, ["foo"]), (2, ["bar"]), (3, ["baz"]), (10, ["biz"])]
  it "adds the value to the end of the list if the key exists" $ do
    insertAppended 3 "biz" m `shouldBe` Map.fromList [(1, ["foo"]), (2, ["bar"]), (3, ["baz", "biz"])]
    insertAppended 1 "biz" m `shouldBe` Map.fromList [(1, ["foo", "biz"]), (2, ["bar"]), (3, ["baz"])]
    (insertAppended 1 "qux" . insertAppended 1 "biz" $ m)
      `shouldBe` Map.fromList [(1, ["foo", "biz", "qux"]), (2, ["bar"]), (3, ["baz"])]

