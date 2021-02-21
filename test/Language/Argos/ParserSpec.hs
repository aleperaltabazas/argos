module Language.Argos.ParserSpec
  ( spec
  )
where

import Data.Argos
import Language.Argos.Parser
import Text.Parsec
import Test.Hspec

isLeft (Right _) = False
isLeft (Left  _) = True

spec :: Spec
spec = describe "Language.Argos.Parser" $ do
  describe "longParser" $ do
    it "works" $ do
      parse longParser "long" "long(blah)" `shouldBe` Right "blah"
      parse longParser "long" "long(foo-bar)" `shouldBe` Right "foo-bar"
      parse longParser "long" "long     (foo-bar     )" `shouldBe` Right "foo-bar"
      parse longParser "long" "long ( foo_bar )" `shouldBe` Right "foo_bar"
    it "fails" $ do
      parse longParser "long" "long(blah" `shouldSatisfy` isLeft
      parse longParser "long" "long()" `shouldSatisfy` isLeft
      parse longParser "long" "long(blah())" `shouldSatisfy` isLeft
  describe "shortParser" $ do
    it "works" $ do
      parse shortParser "short" "short(b)" `shouldBe` Right 'b'
      parse shortParser "short" "short(c)" `shouldBe` Right 'c'
    it "fails" $ do
      parse shortParser "short" "short()" `shouldSatisfy` isLeft
      parse shortParser "short" "short(2)" `shouldSatisfy` isLeft
  describe "optionParser" $ do
    it "works" $ do
      parse optionParser "option" "option(long(blah))" `shouldBe` Right Option { long = "blah", short = Nothing }
      parse optionParser "option" "option(long(blah), short(b))" `shouldBe` Right Option { long = "blah", short = Just 'b' }
    it "fails" $ do
      parse optionParser "option" "option()" `shouldSatisfy` isLeft
      parse optionParser "option" "option(short(b), long(blah))" `shouldSatisfy` isLeft
  describe "commandParser" $ do
    it "works" $ do
      parse commandParser "command" "command(blah){}" `shouldBe` Right Command { name = "blah", arguments = [] }
      parse commandParser "command" "command(blah){option(long(foo))}"
        `shouldBe` Right Command { name = "blah", arguments = [Option { long = "foo", short = Nothing }] }
      parse commandParser "command" "command(blah){\ncommand(foo){}\n}"
        `shouldBe` Right Command { name = "blah", arguments = [Command { name = "foo", arguments = [] }] }
    it "fails" $ do
      parse commandParser "command" "command() {}" `shouldSatisfy` isLeft
      parse commandParser "command" "command(blah)" `shouldSatisfy` isLeft
