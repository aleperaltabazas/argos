module Language.Argos.AutoCompleteSpec where

import Language.Argos.AutoComplete
import Test.Hspec

spec :: Spec
spec =
  describe "Language.Argos.AutoComplete"
    $ describe "autocompleteScript"
    $ it "correctly generates the argos autocomplete script"
    $ do
        argos    <- parseArgosFile "argos.argos"
        expected <- readFile "test/argos-completion.bash"
        (autocompleteScript "argos" <$> argos) `shouldBe` Right expected
