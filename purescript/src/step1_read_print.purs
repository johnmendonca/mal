module Step1ReadPrint where

import Prelude
import Data.String (joinWith)
import Control.Monad.Eff (Eff) 
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Node.ReadLine (READLINE, Interface, createConsoleInterface, noCompletion, setLineHandler, setPrompt, prompt)

import Reader (tokenize)

read :: String -> Array String
read s = tokenize s

eval :: Array String -> Array String
eval s = s

print :: Array String -> String
print s = joinWith " " ( (\x -> "'" ++ x ++ "'") <$> s)

rep :: String -> String
rep s = (read >>> eval >>> print) s

rep_loop :: forall e. Interface -> String -> Eff (console :: CONSOLE, readline :: READLINE | e) Unit
rep_loop i s = do
  log $ rep s
  prompt i

main :: forall e. Eff (console :: CONSOLE, readline :: READLINE, err :: EXCEPTION | e) Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt "user> " 0 interface
  setLineHandler interface (rep_loop interface)
  prompt interface

