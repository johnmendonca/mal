module Step0Repl where

import Prelude
import Control.Monad.Eff (Eff) 
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Node.ReadLine (READLINE, Interface, createConsoleInterface, noCompletion, setLineHandler, setPrompt, prompt)

read :: String -> String
read s = s

eval :: String -> String
eval s = s

print :: String -> String
print s = s

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

