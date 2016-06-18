module Reader (read_str) where

import Prelude
import Data.Array (catMaybes, filter, head, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(Tuple), snd, curry)
import Data.String (trim, null)
import Data.String.Regex (Regex, match, noFlags, regex)
import Data.Int (fromString)

import MalType (MalType(..))

type Tokens = Array String

tail :: forall a. Array a -> Array a
tail a = case Data.Array.tail a of
  Just xs -> xs
  Nothing -> []

read_str :: String -> MalType
read_str s = snd $ read_form (tokenize s)

tokenRegex :: Regex
tokenRegex = regex "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)" noFlags { global = true }

tokenize :: String -> Tokens
tokenize s = filter (not <<< null) $ trim <$> maybe [] catMaybes (match tokenRegex s)

read_form :: Tokens -> Tuple Tokens MalType
read_form ts =
  case head ts of
    Just "(" -> read_list ts
    Just s   -> Tuple (tail ts) (read_atom s)
    Nothing  -> Tuple (tail ts) (MalString "")

read_list :: Tokens -> Tuple Tokens MalType
read_list ts = curry read_list' (tail ts) (MalList [])

read_list' :: Tuple Tokens MalType -> Tuple Tokens MalType
read_list' (Tuple ts m) =
  case head ts of
    Just ")" -> Tuple (tail ts) m
    Just s   -> read_list' $ map (into_mlist m) (read_form ts)
    Nothing  -> Tuple [] (MalString "Error: missing )")

into_mlist :: MalType -> MalType -> MalType
into_mlist (MalList arr) m = MalList (snoc arr m)
into_mlist _             _ = MalString "Error: Expected list type."

read_atom :: String -> MalType
read_atom s =
  case fromString s of
    Just i  -> MalInt i
    Nothing -> MalSymbol s

