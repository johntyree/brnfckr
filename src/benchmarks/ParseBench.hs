{-# LANGUAGE BangPatterns #-}


import System.Directory
import System.FilePath
import Control.DeepSeq

import Criterion.Main
import Brnfckr.Parse (parseBrainFuck)
import Brnfckr.Eval

import Paths_brnfckr

sourceBlacklist :: [FilePath]
sourceBlacklist = [
    "tests.bf"
  ]
runBlacklist = sourceBlacklist ++ [
    "mandelbrot.bf"
  -- , "bottles.bf"
  , "rot13.bf"
  , "bench.bf"
  , "bench2.bf"
  ]

programNames :: IO (FilePath, [FilePath])
programNames = do
  dir <- getDataFileName "programs"
  files <- getDirectoryContents dir
  return $ (dir, filter isNormalProgram files)
    where
      isNormalProgram f = takeExtension f == ".bf"
                       && takeFileName f == "bottles.bf"

readFile' :: FilePath -> IO String
readFile' fn = do
  c <- readFile fn
  return (force c)

main :: IO ()
main = do
  (dir, names) <- programNames
  contents <- mapM (readFile' . (dir </>)) names
  print names
  let sources = zip names contents
      asts = fmap parseBrainFuck <$> sources
      opt_asts = fmap (fmap compress) <$> asts
      makeBenches f progs bl =
        [bench name $ nf f prg | (name, prg) <- progs, name `notElem` bl]
      forceRun ast = let ((_, _), output) = runBrainFuck' ast []
                      in output
  defaultMain [
      bgroup "Parsing" $ makeBenches parseBrainFuck sources sourceBlacklist
    , bgroup "Optimizing" $ makeBenches (fmap compress) asts sourceBlacklist
    , bgroup "Evaluating" $ makeBenches forceRun opt_asts runBlacklist
    ]
