module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Env (initEnv)
import Eval (eval)
import Node.ReadLine (READLINE, close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)
import Parser.Calc (parseExp)
import Prelude (Unit, bind, discard, ($), (*>), (==))
import Text.Parsing.StringParser (runParser)

main :: forall eff. Eff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | eff) Unit
main = do
  interface <- createConsoleInterface noCompletion
  printPrompt interface
  setLineHandler interface $ \s -> loop s initEnv interface
  where
    printPrompt line = setPrompt "> " 2 line *> prompt line
    loop str env interface =
      if str == ":q"
        then close interface
        else case runParser parseExp str of
               Left err -> logShow err
               Right exp ->
                 case eval exp env of
                   Left err -> logShow err
                   Right res -> do
                     logShow res.res
                     printPrompt interface
                     setLineHandler interface $ \s -> loop s res.env interface
