module Env where

import Prelude
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import MalType (MalType(..))

type Env = StrMap MalType

env_lookup :: Env -> String -> MalType
env_lookup env s = 
  case lookup s env of
    Just x  -> x
    Nothing -> MalString "Error: undefined symbol"
    
defaultEnv :: Env
defaultEnv = fromFoldable $ 
  (map >>> map) MalString
    [Tuple "+" "plus_fn",
     Tuple "-" "sub_fn",
     Tuple "*" "mult_fn",
     Tuple "/" "div_fn"]

