module Main where

import           Server

main :: IO ()
main = do
  sock <- tcpListener 9999
  acceptLoop sock mainHandler
