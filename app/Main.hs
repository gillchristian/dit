module Main where

import qualified CLI
import qualified Lib

main :: IO ()
main = do
  cmd <- CLI.run

  print cmd

  Lib.run
