module Main where

import qualified CLI
import qualified Lib

main :: IO ()
main = do
  cmd <- CLI.run

  Lib.run $ CLI.optsConfig cmd

  print cmd
