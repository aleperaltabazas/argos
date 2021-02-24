module Language.Argos.AutoComplete
  ( parseArgos
  , parseArgosFile
  , autocompleteScript
  )
where

import Control.Monad ((=<<))
import Data.Argos (ArgosTree, spread, merge)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Argos.AutoComplete.Writer (writeNode)
import Language.Argos.Parser (argosParser)
import Text.Parsec (ParseError, parse)

parseArgos :: String -> Either ParseError ArgosTree
parseArgos = (foldl merge Map.empty . map spread <$>) . parse argosParser "argos"

parseArgosFile :: FilePath -> IO (Either ParseError ArgosTree)
parseArgosFile = (parseArgos <$>) . readFile

autocompleteScript :: String -> ArgosTree -> String
autocompleteScript progName tree =
  "#/usr/bin/env bash\n\
\"
    ++ progNameCompletion
    ++ "()\n\
\{\n\
\  local cur prev\n\
\\n\
\  cur=${COMP_WORDS[COMP_CWORD]}\n\
\  prev=${COMP_WORDS[COMP_CWORD-1]}\n\
\\n\
\  case ${COMP_CWORD} in\n"
    ++ (intercalate "\n" . mapToList writeNode $ tree)
    ++ "\n\
\    *)\n\
\      COMPREPLY=()\n\
\      ;;\n\
\  esac\n\
\}\n\
\\n\
\complete -F "
    ++ progNameCompletion
    ++ " "
    ++ progName
  where progNameCompletion = "_" ++ progName ++ "_completions"

mapToList :: (k -> v -> u) -> Map k v -> [u]
mapToList f = map (uncurry f) . Map.toList

