module Reader (Reader, read_str) where

import Prelude
import Data.Array (catMaybes, filter, index, snoc)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(Tuple), snd, curry, uncurry)
import Data.String (trim, null)
import Data.String.Regex (Regex, match, noFlags, regex)
import Data.Int (fromString)

import MalType (MalType(..))

type Reader = Tuple (Array String) Int

peek :: Reader -> Maybe String
peek r = uncurry index r

next :: Reader -> Reader
next r = map (add 1) r

read_str :: String -> MalType
read_str s = snd $ curry read_form (tokenize s) 0

tokenRegex :: Regex
tokenRegex = regex "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)" noFlags { global = true }

tokenize :: String -> Array String
tokenize s = filter (not <<< null) $ trim <$> maybe [] catMaybes (match tokenRegex s)

read_form :: Reader -> Tuple Reader MalType
read_form rdr =
  case peek rdr of
    Just "(" -> read_list rdr
    Just s   -> read_atom rdr
    Nothing  -> read_nothing rdr

read_list :: Reader -> Tuple Reader MalType
read_list rdr = curry read_list' (next rdr) (MalList [])

read_list' :: Tuple Reader MalType -> Tuple Reader MalType
read_list' (Tuple rdr m) =
  case peek rdr of
    Just ")" -> Tuple (next rdr) m
    Just s   -> read_list' $ map (into_mlist m) (read_form rdr)
    Nothing  -> Tuple (next rdr) (MalString "Error: missing )")

into_mlist :: MalType -> MalType -> MalType
into_mlist (MalList arr) m = MalList (snoc arr m)
into_mlist _             _ = MalString "Error: Expected list type."

read_atom :: Reader -> Tuple Reader MalType
read_atom rdr =
  case peek rdr of
    Just s   -> Tuple (next rdr) (read_atom_str s)
    Nothing  -> Tuple (next rdr) MalNil

read_atom_str :: String -> MalType
read_atom_str s =
  case fromString s of
    Just i  -> MalInt i
    Nothing -> MalSymbol s

read_nothing :: Reader -> Tuple Reader MalType
read_nothing rdr = Tuple (next rdr) MalNil

