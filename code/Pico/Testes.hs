module Pico.Testes where

import Pico.Syntax
import Pico.Interpreter

import Test.HUnit

sp :: Program
sp = Program st [x10, y50, res]

st :: SymbolTable
st = [("x", TNatural), ("y", TNatural), ("output", TNatural)]

x10 :: Statement
x10 = Assignment "x" (ExpValue (NATValue 10))

y50 :: Statement
y50 = Assignment "y" (ExpValue (NATValue 50))

res :: Statement
res = Assignment "output" (Add (Var "x") (Var "y"))

test1 = TestCase (assertEqual "for x = 10; y = 50; output = x + y;" (NATValue 60) (runProgram sp)) 
