{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.Exit
-- import System.Remote.Monitoring

import Brnfckr.Eval (runBrainFuck)

main :: IO ()
main = do
  -- forkServer "localhost" 8000
  fname <- fmap head getArgs
  source <- readFile fname
  stream <- getContents
  case runBrainFuck source stream of
    ((Left e, _), _) -> print e >> exitWith (ExitFailure 1)
    ((_, _), s) -> putStr s
