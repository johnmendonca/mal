module Step2Eval where

import Prelude
import Data.Array (null, head, tail)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff) 
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Node.ReadLine (READLINE, Interface, createConsoleInterface, noCompletion, setLineHandler, setPrompt, prompt)

import MalType (MalType(..))
import Reader (read_str)
import Env (Env, env_lookup, defaultEnv)

read :: String -> MalType
read s = read_str s

eval :: Env -> MalType -> MalType
eval env ml@(MalList l) | not (null l) = apply_fn $ eval_ast env ml
eval env s                             =            eval_ast env s

eval_ast :: Env -> MalType -> MalType
eval_ast env (MalSymbol str) = env_lookup env str
eval_ast env (MalList list)  = MalList $ (eval env) <$> list
eval_ast _   x               = x

apply_fn :: MalType -> MalType
apply_fn ml@(MalList l) =
  case head l of
    Just (MalFn fn) ->
      case tail l of
       Just xs -> fn xs
       Nothing -> fn []
    Just x          -> MalString ("Error: cannot apply " <> show x)
    Nothing         -> ml
apply_fn x = MalString  ("Error: cannot apply " <> show x)

print :: MalType -> String
print a = show a

rep :: String -> String
rep s = (read >>> (eval defaultEnv) >>> print) s

rep_loop :: forall e. Interface -> String -> Eff (console :: CONSOLE, readline :: READLINE | e) Unit
rep_loop i s = do
  log $ s
  log $ rep s
  prompt i

main :: forall e. Eff (console :: CONSOLE, readline :: READLINE, exception :: EXCEPTION | e) Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt "user> " 0 interface
  setLineHandler interface (rep_loop interface)
  prompt interface

