module MalType where

import Prelude
import Data.String (joinWith)
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Traversable (sequence)

data MalType = MalList    (Array MalType)
             | MalFn      (Array MalType -> MalType)
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
  show (MalFn a)     = "fn:"

insert_into :: MalType -> MalType -> MalType
insert_into (MalList arr) m = MalList (snoc arr m)
insert_into _             _ = MalString "Error: Expected list type."

intFn :: (Array Int -> Int) -> Array MalType -> MalType
intFn fn = \ms ->
  case toInts ms of
    Left str   -> MalString str
    Right ints -> MalInt $ fn ints

toInts :: Array MalType -> Either String (Array Int)
toInts ms = sequence (toEither <$> ms)
  where
    toEither (MalInt x) = Right x
    toEither x          = Left ("Error: expected integer but received " <> show x)

