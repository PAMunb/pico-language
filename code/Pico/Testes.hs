module Pico.Testes where

import Pico.Syntax
import Pico.Interpreter

import Test.HUnit

st :: SymbolTable
st = [("x", TNatural), ("y", TNatural), ("output", TNatural)]

testSuite = TestList [ 
  TestLabel "sumTest" sumTest, 
  TestLabel "subTest" subTest,
  TestLabel "subTest" subTest,
  TestLabel "multTest" multTest,
  TestLabel "divTest" divTest,
  TestLabel "powTest" powTest,
  TestLabel "maxTest" maxTest,
  TestLabel "minTest" minTest, 
  TestLabel "concatTest" concatTest, 
  TestLabel "ifThenTest" ifThenTest,
  TestLabel "ifThenElseTest1" ifThenElseTest1,
  TestLabel "ifThenElseTest2" ifThenElseTest2,
  TestLabel "whileTest1" whileTest1,
  TestLabel "whileTest2" whileTest2 ]

-- | Variables for sum and sub tests

y03 :: Statement
y03 = Assignment "y" (ExpValue (NATValue 3))

x10 :: Statement
x10 = Assignment "x" (ExpValue (NATValue 10))

y50 :: Statement
y50 = Assignment "y" (ExpValue (NATValue 50))

string1 :: Statement
string1 = Assignment "x" (ExpValue (STRValue "static"))

string2 :: Statement
string2 = Assignment "y" (ExpValue (STRValue "analysis"))

-- | Simple sum statement test
sumProgram :: Program
sumProgram = Program st [x10, y50, resultSum]

resultSum :: Statement
resultSum = Assignment "output" (Add (Var "x") (Var "y"))

sumTest = TestCase (assertEqual "for x = 10; y = 50; output = x + y;" (NATValue 60) (runProgram sumProgram)) 

-- | Simple subtraction statement test
subProgram :: Program
subProgram = Program st [x10, y50, resultSub] 

resultSub :: Statement
resultSub = Assignment "output" (Sub (Var "y") (Var "x"))

subTest = TestCase (assertEqual "for x = 10; y = 50; output = y - x;" (NATValue 40) (runProgram subProgram))

-- | Simple multiplication statement test
multProgram :: Program
multProgram = Program st [x10, y50, resultMult] 

resultMult :: Statement
resultMult = Assignment "output" (Mult (Var "y") (Var "x"))

multTest = TestCase (assertEqual "for x = 10; y = 50; output = y * x;" (NATValue 500) (runProgram multProgram))

-- | Simple division statement test
divProgram :: Program
divProgram = Program st [x10, y03, resultDiv]

resultDiv :: Statement
resultDiv = Assignment "output" (Div (Var "x") (Var "y"))

divTest = TestCase (assertEqual "for x = 10; y = 3; output = x / y;" (NATValue 3) (runProgram divProgram))

-- | Simple exponentiation statement test
powProgram :: Program
powProgram = Program st [x10, y03, resultPow]

resultPow :: Statement
resultPow = Assignment "output" (Pow (Var "x") (Var "y"))

powTest = TestCase (assertEqual "for x = 10; y = 3; output = x ^ y;" (NATValue 1000) (runProgram powProgram))

-- | Simple maximum number test
maxProgram :: Program
maxProgram = Program st [x10, y03, resultMax]

resultMax :: Statement
resultMax = Assignment "output" (Max (Var "x") (Var "y"))

maxTest = TestCase (assertEqual "for x = 10; y = 3; output = >(x, y);" (NATValue 10) (runProgram maxProgram))

-- | Simple minimum number test
minProgram :: Program
minProgram = Program st [x10, y03, resultMin]

resultMin :: Statement
resultMin = Assignment "output" (Min (Var "x") (Var "y"))

minTest = TestCase (assertEqual "for x = 10; y = 3; output = <(x, y);" (NATValue 3) (runProgram minProgram))

-- | Simple concat string test
concatProgram :: Program
concatProgram = Program st [string1, string2, resultConcat]

resultConcat :: Statement
resultConcat = Assignment "output" (Concat (Var "x") (Var "y"))

concatTest = TestCase (assertEqual "for x = \"static\"; y = \"analysis\"; output = x | y;" (STRValue "staticanalysis") (runProgram concatProgram))


-- | Simple ifThen statement test
ifThenProgram :: Program
ifThenProgram = Program st [out10, ifBlock]

out10 :: Statement
out10 = Assignment "output" (ExpValue (NATValue 10))

ifBlock :: Statement
ifBlock = IfThen trueExp ifBody

ifBody :: Statement
ifBody = Assignment "output" (ExpValue (NATValue 20))

ifThenTest = TestCase (assertEqual "for output = 10; if(1){ output = 20 }" 
                      (NATValue 20) (runProgram ifThenProgram)) 

-- | Simple ifThenElse statement test
ifThenElseProgram1 :: Program
ifThenElseProgram1 = Program st [ifElseBlock1]

ifElseBlock1 :: Statement
ifElseBlock1 = IfThenElse trueExp statement1 statement2

ifThenElseProgram2 :: Program
ifThenElseProgram2 = Program st [ifElseBlock2]

ifElseBlock2 :: Statement
ifElseBlock2 = IfThenElse falseExp statement1 statement2

statement1 :: Statement
statement1 = Assignment "output" (ExpValue (NATValue 1))

statement2 :: Statement
statement2 = Assignment "output" (ExpValue (NATValue 2))

-- Test for Then clause
ifThenElseTest1 = TestCase (assertEqual "if(1){output = 1} else {output = 2}" 
                           (NATValue 1) (runProgram ifThenElseProgram1)) 
-- Test for Else clause
ifThenElseTest2 = TestCase (assertEqual "if(0){output = 1} else {output = 2}" 
                           (NATValue 2) (runProgram ifThenElseProgram2)) 

-- | Simple while statement test
whileProgram1 :: Program
whileProgram1 = Program st [out1, whileBlock1]

out1 :: Statement
out1 = Assignment "output" (ExpValue (NATValue 1))

whileBlock1 :: Statement
whileBlock1 = While falseExp whileStatement

-- Test while block with false expression
whileTest1 = TestCase (assertEqual "output = 1; while(false){...}"
                      (NATValue 1) (runProgram whileProgram1))

whileProgram2 :: Program
whileProgram2 = Program st [out1, x1, whileBlock2]

x1 :: Statement
x1 = Assignment "x" (ExpValue (NATValue 1))

whileBlock2 :: Statement
whileBlock2 = While whileExp whileStatement

whileExp :: Expression
whileExp = Var "x"

whileStatement :: Statement
whileStatement = 
  Block [Assignment "output" (Add (Var "output") (Var "x")),
         Assignment "x" (Sub (Var "x") (Var "x"))]

-- Test while block with var expression
whileTest2 = TestCase (assertEqual "output = 1; x = 1; while(x){output+1; x-1}"
                      (NATValue 2) (runProgram whileProgram2))

-- | Utility functions for tests

-- | True expression
trueExp :: Expression
trueExp = (ExpValue (NATValue 1))

-- | False expression
falseExp :: Expression
falseExp = (ExpValue (NATValue 0))
