module Language.Argos.AutoComplete.WriterSpec where

import Data.Argos
import qualified Data.Map as Map
import Language.Argos.AutoComplete.Writer
import Test.Hspec

spec :: Spec
spec = describe "Language.Argos.AutoComplete.Writer" $ describe "writeNode" $ do
  it "returns the first autocompletion level" $ do
    writeNode 0 [Argos { previous = Nothing, current = "foo bar" }]
      `shouldBe` "    1)\n      COMPREPLY=($(compgen -W \"foo bar\" -- ${cur}))\n      ;;"
    writeNode 0 [Argos { previous = Nothing, current = "foo --bar" }, Argos { previous = Nothing, current = "baz --biz" }]
      `shouldBe` "    1)\n      COMPREPLY=($(compgen -W \"foo --bar baz --biz\" -- ${cur}))\n      ;;"
  it "creates different cases for each Argos in the node based on the each's previous" $ do
    writeNode 1 [Argos { previous = Just "foo", current = "bar --baz" }]
      `shouldBe` "    2)\n      case ${prev} in\n        foo)\n          COMPREPLY=($(compgen -W \"bar --baz\" -- ${cur}))\n          ;;\n      esac\n      ;;"
    writeNode
        2
        [ Argos { previous = Just "foo", current = "-a -c" }
        , Argos { previous = Just "bar", current = "-b -d" }
        , Argos { previous = Just "baz", current = "--argo" }
        ]
      `shouldBe` "    3)\n      case ${prev} in\n        foo)\n          COMPREPLY=($(compgen -W \"-a -c\" -- ${cur}))\n          ;;\n        bar)\n          COMPREPLY=($(compgen -W \"-b -d\" -- ${cur}))\n          ;;\n        baz)\n          COMPREPLY=($(compgen -W \"--argo\" -- ${cur}))\n          ;;\n      esac\n      ;;"
