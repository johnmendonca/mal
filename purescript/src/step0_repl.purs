module Step0Repl where

import Prelude
import Control.Monad.Eff (Eff) 
import Control.Monad.Eff.Console (CONSOLE)

import Node.ReadLine (READLINE)
import Node.SimpleRepl (setPrompt, readLine, runRepl, putStrLn)

main :: forall e. Eff (console :: CONSOLE, readline :: READLINE | e) Unit
main = runRepl do
  setPrompt "mal-user> "
  loop
  where
    loop = do
      res <- readLine
      putStrLn $ res
      putStrLn $ res
      loop

