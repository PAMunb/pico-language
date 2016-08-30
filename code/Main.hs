module Main where

import System.IO
import Pico.Parser
import Pico.Syntax

main = do
  file <- getLine
  contents <- readFile file 
  let p = parsePicoProgram contents
  putStrLn $ show p
