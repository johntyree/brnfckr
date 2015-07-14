
module Main where

import System.Environment
import System.Exit

import Brnfckr.Eval (runBrainFuck)

main :: IO ()
main = do
  fname <- head <$> getArgs
  source <- readFile fname
  stream <- getContents
  case runBrainFuck source stream of
    ((Left e, _), _) -> print e >> exitWith (ExitFailure 1)
    ((_, _), s) -> putStr s
