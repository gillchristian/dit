module Main where

import qualified Dit.CLI as CLI
import qualified Dit.Lib as Lib

main :: IO ()
main = do
  cmd <- CLI.run

  Lib.run $ CLI.optsConfig cmd

  print cmd
