module Pico.Testes where

import Pico.Syntax
import Pico.Interpreter

import Test.HUnit

st :: SymbolTable
st = [("x", TNatural), ("y", TNatural), ("output", TNatural)]

testSuite = 
  TestList [ TestLabel "sumTest" sumTest, 
             TestLabel "ifThenTest" ifThenTest ]

-- | Simple sum statement test
sumProgram :: Program
sumProgram = Program st [x10, y50, res]

x10 :: Statement
x10 = Assignment "x" (ExpValue (NATValue 10))

y50 :: Statement
y50 = Assignment "y" (ExpValue (NATValue 50))

res :: Statement
res = Assignment "output" (Add (Var "x") (Var "y"))

sumTest = TestCase (assertEqual "for x = 10; y = 50; output = x + y;" (NATValue 60) (runProgram sumProgram)) 

-- | Simple ifThen statement test
ifThenProgram :: Program
ifThenProgram = Program st [out10, ifBlock]

out10 :: Statement
out10 = Assignment "output" (ExpValue (NATValue 10))

ifBlock :: Statement
ifBlock = IfThen ifExp ifBody

ifExp :: Expression
ifExp = (ExpValue (NATValue 1))

ifBody :: Statement
ifBody = Assignment "output" (ExpValue (NATValue 20))

ifThenTest = TestCase (assertEqual "for output = 10; if(1){ output = 20 }" (NATValue 20) (runProgram ifThenProgram)) 
