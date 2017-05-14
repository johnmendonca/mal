module Env where

import Prelude
import Data.StrMap (StrMap, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Data.Foldable (foldl, foldr)
import Data.Array (head, tail)
import MalType (MalType(..), intFn)

type Env = StrMap MalType

env_lookup :: Env -> String -> MalType
env_lookup env s = 
  case lookup s env of
    Just x  -> x
    Nothing -> MalString "Error: undefined symbol"
    
subn :: Array Int -> Int
subn [x] = (-x)
subn xs  = foldr (-) 0 xs

divn :: Array Int -> Int
divn xs  =
  case head xs of
    Just x ->
      case tail xs of
        Just []  -> 1/x
        Just [y] -> x/y
        Just ys  -> foldl (/) x ys
        Nothing  -> 1
    Nothing -> 1

defaultEnv :: Env
defaultEnv = fromFoldable $ 
  (map >>> map) MalFn
    [Tuple "+" (intFn (foldl (+) 0)),
     Tuple "*" (intFn (foldl (*) 1)),
     Tuple "-" (intFn subn),
     Tuple "/" (intFn divn)]

