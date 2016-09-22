module Pico.Testes where

import Prelude hiding (GTE, LTE)
import System.IO

import Pico.LexPico
import Pico.ParPico
import Pico.SkelPico
import Pico.PrintPico
import Pico.ErrM

import Pico.AbsPico
import Pico.Interpreter
import Pico.Parser hiding (assignment, var)

import Test.HUnit

st :: SymbolTable
st = [ Decl (Ident "x") TInteger
     , Decl (Ident "y") TInteger
     , Decl (Ident "output") TInteger
     ]

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
  TestLabel "whileTest2" whileTest2,
  TestLabel "parseTest1" parseTest1,
  TestLabel "parseTest2" parseTest2
  ]

-- | Variables for sum and sub tests

assignment str exp = Assignment (Ident str) exp
var str = Var (Ident str)

y03 :: Stmt
y03 = assignment "y" (EXPValue (INTValue 3))

x10 :: Stmt
x10 = assignment "x" (EXPValue (INTValue 10))

y50 :: Stmt
y50 = assignment "y" (EXPValue (INTValue 50))

string1 :: Stmt
string1 = assignment "x" (EXPValue (STRValue "static"))

string2 :: Stmt
string2 = assignment "y" (EXPValue (STRValue "analysis"))

-- | Simple sum statement test
sumProgram :: Program
sumProgram = Program st [x10, y50, resultSum]

resultSum :: Stmt
resultSum = assignment "output" (Add (var "x") (var "y"))

sumTest = TestCase (assertEqual "for x = 10; y = 50; output = x + y;" (INTValue 60) (runProgram sumProgram)) 

-- | Simple subtraction statement test
subProgram :: Program
subProgram = Program st [x10, y50, resultSub] 

resultSub :: Stmt
resultSub = assignment "output" (Sub (var "y") (var "x"))

subTest = TestCase (assertEqual "for x = 10; y = 50; output = y - x;" (INTValue 40) (runProgram subProgram))

-- | Simple multiplication statement test
multProgram :: Program
multProgram = Program st [x10, y50, resultMult] 

resultMult :: Stmt
resultMult = assignment "output" (Mult (var "y") (var "x"))

multTest = TestCase (assertEqual "for x = 10; y = 50; output = y * x;" (INTValue 500) (runProgram multProgram))

-- | Simple division statement test
divProgram :: Program
divProgram = Program st [x10, y03, resultDiv]

resultDiv :: Stmt
resultDiv = assignment "output" (Div (var "x") (var "y"))

divTest = TestCase (assertEqual "for x = 10; y = 3; output = x / y;" (INTValue 3) (runProgram divProgram))

-- | Simple exponentiation statement test
powProgram :: Program
powProgram = Program st [x10, y03, resultPow]

resultPow :: Stmt
resultPow = assignment "output" (Pow (var "x") (var "y"))

powTest = TestCase (assertEqual "for x = 10; y = 3; output = x ^ y;" (INTValue 1000) (runProgram powProgram))

-- | Simple maximum number test
maxProgram :: Program
maxProgram = Program st [x10, y03, resultMax]

resultMax :: Stmt
resultMax = assignment "output" (GTE (var "x") (var "y"))

maxTest = TestCase (assertEqual "for x = 10; y = 3; output = >(x, y);" (INTValue 10) (runProgram maxProgram))

-- | Simple minimum number test
minProgram :: Program
minProgram = Program st [x10, y03, resultMin]

resultMin :: Stmt
resultMin = assignment "output" (LTE (var "x") (var "y"))

minTest = TestCase (assertEqual "for x = 10; y = 3; output = <(x, y);" (INTValue 3) (runProgram minProgram))

-- | Simple concat string test
concatProgram :: Program
concatProgram = Program st [string1, string2, resultConcat]

resultConcat :: Stmt
resultConcat = assignment "output" (Concat (var "x") (var "y"))

concatTest = TestCase (assertEqual "for x = \"static\"; y = \"analysis\"; output = x | y;" (STRValue "staticanalysis") (runProgram concatProgram))


-- | Simple ifThen statement test
ifThenProgram :: Program
ifThenProgram = Program st [out10, ifBlock]

out10 :: Stmt
out10 = assignment "output" (EXPValue (INTValue 10))

ifBlock :: Stmt
ifBlock = IfThen trueExp ifBody

ifBody :: Stmt
ifBody = assignment "output" (EXPValue (INTValue 20))

ifThenTest = TestCase (assertEqual "for output = 10; if(1){ output = 20 }" 
                      (INTValue 20) (runProgram ifThenProgram)) 

-- | Simple ifThenElse statement test
ifThenElseProgram1 :: Program
ifThenElseProgram1 = Program st [ifElseBlock1]

ifElseBlock1 :: Stmt
ifElseBlock1 = IfThenElse trueExp statement1 statement2

ifThenElseProgram2 :: Program
ifThenElseProgram2 = Program st [ifElseBlock2]

ifElseBlock2 :: Stmt
ifElseBlock2 = IfThenElse falseExp statement1 statement2

statement1 :: Stmt
statement1 = assignment "output" (EXPValue (INTValue 1))

statement2 :: Stmt
statement2 = assignment "output" (EXPValue (INTValue 2))

-- Test for Then clause
ifThenElseTest1 = TestCase (assertEqual "if(1){output = 1} else {output = 2}" 
                           (INTValue 1) (runProgram ifThenElseProgram1)) 
-- Test for Else clause
ifThenElseTest2 = TestCase (assertEqual "if(0){output = 1} else {output = 2}" 
                           (INTValue 2) (runProgram ifThenElseProgram2)) 

-- | Simple while statement test
whileProgram1 :: Program
whileProgram1 = Program st [out1, whileBlock1]

out1 :: Stmt
out1 = assignment "output" (EXPValue (INTValue 1))

whileBlock1 :: Stmt
whileBlock1 = While falseExp whileStmt

-- Test while block with false expression
whileTest1 = TestCase (assertEqual "output = 1; while(false){...}"
                      (INTValue 1) (runProgram whileProgram1))

whileProgram2 :: Program
whileProgram2 = Program st [out1, x1, whileBlock2]

x1 :: Stmt
x1 = assignment "x" (EXPValue (INTValue 1))

whileBlock2 :: Stmt
whileBlock2 = While whileExp whileStmt

whileExp :: Expression
whileExp = var "x"

whileStmt :: Stmt
whileStmt = 
  Block [assignment "output" (Add (var "output") (var "x")),
         assignment "x" (Sub (var "x") (var "x"))]

-- Test while block with var expression
whileTest2 = TestCase (assertEqual "output = 1; x = 1; while(x){output+1; x-1}"
                      (INTValue 2) (runProgram whileProgram2))

-- | Utility functions for tests

-- | True expression
trueExp :: Expression
trueExp = (EXPValue (INTValue 1))

-- | False expression
falseExp :: Expression
falseExp = (EXPValue (INTValue 0))


parseTest1 = TestCase (do
                         contents <- readFile "../samples/soma.pico"
                         let p = parsePicoProgram contents
                         case p of
                           [("", Program _ _)] -> assertBool "Expecting parsing ok " True
                           otherwise -> assertFailure "File: ../samples/somaPico should be ok."
  )

  

parseTest2 = TestCase (do
                         contents <- readFile "../samples/times.pico"
                         let p = parsePicoProgram contents
                         case p of
                           [("", Program _ _)] -> assertBool "Expecting parsing ok " True
                           otherwise -> assertFailure "File: ../samples/timesPico should be ok."
  )


parseBNFC = TestCase (do
                       contents <- readFile "../samples/times.pico"
                       let ts = myLexer contents
                       let res = pProgram ts
                       case res of
                         Bad s -> assertFailure ("File: ../samples/timesPico should be ok." ++ (show s))
                         Ok tree -> assertBool "Expecting parsing ok " True
  )
