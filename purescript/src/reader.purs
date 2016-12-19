module Reader (read_str, tokenize) where

import Prelude
import Data.Array (filter, head)
import Data.Array (tail) as Data.Array
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(Tuple), snd)
import Data.String (null)
import Data.String.Regex (Regex, split, regex)
import Data.String.Regex.Flags (global)
import Data.Int (fromString)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import MalType (MalType(..), insert_into)

type Tokens = Array String

tail :: forall a. Array a -> Array a
tail a = case Data.Array.tail a of
  Just xs -> xs
  Nothing -> []

read_str :: String -> MalType
read_str s = snd $ read_form (tokenize s)

tokenRegex :: Regex
tokenRegex =
  case regexDef of
    Left str    -> unsafeThrow str
    Right regex -> regex
  where
    regexDef = regex "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)" global

tokenize :: String -> Tokens
tokenize s = filter (not <<< null) $ split tokenRegex s

read_form :: Tokens -> Tuple Tokens MalType
read_form ts =
  case head ts of
    Just "(" -> read_list $ Tuple (tail ts) (MalList [])
    Just s   ->             Tuple (tail ts) (read_atom s)
    Nothing  ->             Tuple (tail ts) (MalString "")

read_list :: Tuple Tokens MalType -> Tuple Tokens MalType
read_list (Tuple ts list) =
  case head ts of
    Just ")" -> Tuple (tail ts) list
    Just s   -> read_list $ map (insert_into list) (read_form ts)
    Nothing  -> Tuple [] (MalString "Error: missing )")

read_atom :: String -> MalType
read_atom s =
  case fromString s of
    Just i  -> MalInt i
    Nothing -> MalSymbol s

