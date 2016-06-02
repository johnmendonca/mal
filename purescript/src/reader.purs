module Reader (tokenize) where

import Prelude
import Data.Array (catMaybes, filter)
import Data.Maybe (maybe)
import Data.String (trim, null)
import Data.String.Regex (Regex, match, noFlags, regex)

tokenRegex :: Regex
tokenRegex = regex "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)" noFlags { global = true }

tokenize :: String -> Array String
tokenize s = filter (not <<< null) $ trim <$> maybe [] catMaybes (match tokenRegex s)

