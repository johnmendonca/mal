module MalType where

import Prelude
import Data.String (joinWith)
import Data.Array (snoc)

data MalType = MalList    (Array MalType)
             | MalSymbol  String
             | MalInt     Int
             | MalString  String
             | MalNil

instance showMalType :: Show MalType where
  show (MalList a)   = "(" <> (joinWith " " $ show <$> a) <> ")"
  show (MalSymbol a) = a
  show (MalString a) = "\"" <> a <> "\""
  show (MalInt a)    = show a
  show MalNil        = "nil"

insert_into :: MalType -> MalType -> MalType
insert_into (MalList arr) m = MalList (snoc arr m)
insert_into _             _ = MalString "Error: Expected list type."

