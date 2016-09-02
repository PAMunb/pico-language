module Pico.Testes where

import Pico.Syntax
import Pico.Interpreter

import Test.HUnit

soma :: Program
soma = Program st [x10, y50, res1]

subtracao :: Program
subtracao = Program st [x10, y50, res2] 

multiplicacao :: Program
multiplicacao = Program st [x10, y50, res3] 

potencia :: Program
potencia = Program st [x10, y03, res4]

divisao :: Program
divisao = Program st [x10, y03, res5]

st :: SymbolTable
st = [("x", TNatural), ("y", TNatural), ("output", TNatural)]

y03 :: Statement
y03 = Assignment "y" (ExpValue (NATValue 3))

x10 :: Statement
x10 = Assignment "x" (ExpValue (NATValue 10))

y50 :: Statement
y50 = Assignment "y" (ExpValue (NATValue 50))

res1 :: Statement
res1 = Assignment "output" (Add (Var "x") (Var "y"))

res2 :: Statement
res2 = Assignment "output" (Sub (Var "y") (Var "x"))

res3 :: Statement
res3 = Assignment "output" (Mult (Var "y") (Var "x"))

res4 :: Statement
res4 = Assignment "output" (Pow (Var "x") (Var "y"))

res5 :: Statement
res5 = Assignment "output" (Div (Var "x") (Var "y"))

test1 = TestCase (assertEqual "for x = 10; y = 50; output = x + y;" (NATValue 60) (runProgram soma)) 
test2 = TestCase (assertEqual "for x = 10; y = 50; output = y - x;" (NATValue 40) (runProgram subtracao))
test3 = TestCase (assertEqual "for x = 10; y = 50; output = y * x;" (NATValue 500) (runProgram multiplicacao))
test4 = TestCase (assertEqual "for x = 10; y = 3; output = x ^ y;" (NATValue 1000) (runProgram potencia))
test5 = TestCase (assertEqual "for x = 10; y = 10; output = x / y;" (NATValue 3) (runProgram divisao))
test6 = TestCase (assertEqual "while (x > 10)" (NATValue 3) (runProgram divisao))

tests = TestList [TestLabel "soma" test1, 
                  TestLabel "subtracao" test2,
                  TestLabel "multiplicacao" test3,
                  TestLabel "potencia" test4,
                  TestLabel "divisao" test5]
