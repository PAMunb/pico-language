module Pico.StaticAnalysisTest where

import Prelude hiding (GTE, LTE)
import System.IO

import Pico.AbsPico
import Pico.Interpreter
import Pico.StaticAnalyser
import Pico.Parser hiding (assignment, var)

import Test.HUnit

st :: SymbolTable
st = [ Decl (Ident "x") TInteger
     , Decl (Ident "output") TInteger
     ]

testSuite = TestList [ 
  TestLabel "outputUnchangedTest" outputUnchangedTest,
  TestLabel "outputChangedTest" outputChangedTest,
  TestLabel "condOutputChangeTest" condOutputChangeTest,
  TestLabel "ifThenElseOutputTest" ifThenElseOutputTest,
  TestLabel "whileOutputTest" whileOutputTest
  ]

assignment str exp = Assignment (Ident str) exp
var str = Var (Ident str)

-- | Simple outputUnchanged statement test
outputUnchangedProgram :: Program
outputUnchangedProgram = Program st [stubVar]

stubVar :: Stmt
stubVar = assignment "x" (EXPValue (INTValue 10))

outputUnchangedTest = TestCase (assertEqual "x = 10;" 
                      (["Output unchanged"]) (analyseProgram outputUnchangedProgram)) 

-- | Simple outputChanged statement test
outputChangedProgram :: Program
outputChangedProgram = Program st [outputAssign]

outputAssign :: Stmt
outputAssign = assignment "output" (EXPValue (INTValue 10))

outputChangedTest = TestCase (assertEqual "output = 10;" 
                      ([]) (analyseProgram outputChangedProgram)) 

-- | Simple conditional output change statement test
condOutputChangeProgram :: Program
condOutputChangeProgram = Program st [ifBlock]

ifBlock :: Stmt
ifBlock = IfThen trueExp ifBody

ifBody :: Stmt
ifBody = assignment "output" (EXPValue (INTValue 20))

condOutputChangeTest = TestCase (assertEqual "for if(1){ output = 20 }" 
                      ([]) (analyseProgram condOutputChangeProgram)) 

-- | Simple conditional output change with ifThenElse statement test
condIfThenElseProgram :: Program
condIfThenElseProgram = Program st [ifElseBlock]

ifElseBlock :: Stmt
ifElseBlock = IfThenElse trueExp statement1 statement2

statement1 :: Stmt
statement1 = assignment "x" (EXPValue (INTValue 1))

statement2 :: Stmt
statement2 = assignment "output" (EXPValue (INTValue 2))

ifThenElseOutputTest = TestCase (assertEqual "if(0){x = 1} else {output = 2}" 
                           ([]) (analyseProgram condIfThenElseProgram)) 

-- | Simple conditional output change with while statement test
condWhileProgram :: Program
condWhileProgram = Program st [x1, whileBlock]

x1 :: Stmt
x1 = assignment "x" (EXPValue (INTValue 1))

whileBlock :: Stmt
whileBlock = While whileExp whileStmt

whileExp :: Expression
whileExp = var "x"

whileStmt :: Stmt
whileStmt = 
  Block [assignment "output" (Add (var "output") (var "x"))]

-- Test while block with var expression
whileOutputTest = TestCase (assertEqual "x = 1; while(x){output+1}"
                      ([]) (analyseProgram condWhileProgram))

-- | True expression
trueExp :: Expression
trueExp = (EXPValue (INTValue 1))
