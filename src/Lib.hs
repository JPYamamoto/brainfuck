module Lib
    ( main
    ) where

import Interpreter

main :: IO ()
main = do
  putStrLn "Input your program:"
  program <- getLine
  run program
