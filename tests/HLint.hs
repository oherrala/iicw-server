module Main where

import Language.Haskell.HLint3

main :: IO ()
main = do
  hints <- hlint ["src", "tests"]
  print hints
