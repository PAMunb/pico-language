module Pico.Testes where

import Pico.Syntax
import Pico.Interpreter

import Test.HUnit

soma :: Program
soma = Program st [x10, y50, res1]

subtracao :: Program
subtracao = Program st [x10, y50, res2] 

st :: SymbolTable
st = [("x", TNatural), ("y", TNatural), ("output", TNatural)]

x10 :: Statement
x10 = Assignment "x" (ExpValue (NATValue 10))

y50 :: Statement
y50 = Assignment "y" (ExpValue (NATValue 50))

res1 :: Statement
res1 = Assignment "output" (Add (Var "x") (Var "y"))

res2 :: Statement
res2 = Assignment "output" (Sub (Var "y") (Var "x"))

test1 = TestCase (assertEqual "for x = 10; y = 50; output = x + y;" (NATValue 60) (runProgram soma)) 
test2 = TestCase (assertEqual "for x = 10; y = 50; output = y - x;" (NATValue 40) (runProgram subtracao))

tests = TestList [TestLabel "soma" test1, TestLabel "subtracao" test2]
