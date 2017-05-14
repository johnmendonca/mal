module Step1ReadPrint where

import Prelude
import Control.Monad.Eff (Eff) 
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Node.ReadLine (READLINE, Interface, createConsoleInterface, noCompletion, setLineHandler, setPrompt, prompt)

import MalType (MalType)
import Reader (read_str)

read :: String -> MalType
read s = read_str s

eval :: MalType -> MalType
eval s = s

print :: MalType -> String
print a = show a

rep :: String -> String
rep s = (read >>> eval >>> print) s

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

