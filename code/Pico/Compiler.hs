module Pico.Compiler where

import Pico.AbsPico
import Pico.Assembly

-- State: "one or more variables that are required
---- to perform some computation but are not among the
---- arguments of the relevant function."
-- "Functions that consume a state and produce both
---- a result and an updated state, which are given 
---- back in a tuple."
import Control.Monad.State

type LabeledInstructions = State Int [Instr]

nextLabel :: State Int String
nextLabel = do
  curr <- get
  let next = curr + 1
  put next
  return ("L" ++ show next)

-- Compile the program as a list of instructions
compileProgram :: Program -> [Instr]
compileProgram (Program decls stmts) = fst $ runState instr (0::Int)
  where
    instr = do
      instr1 <- compileDeclarations decls
      instr2 <- compileStatements stmts
      return (instr1 ++ instr2)
        
compileDeclarations :: [Decl] -> LabeledInstructions
-- mapM map each element to a monadic action and collect the results.
compileDeclarations decls = mapM compileDeclaration decls
  where
    compileDeclaration (Decl ident TInteger) = return (DclInt ident)
    compileDeclaration (Decl ident TString) = return (DclStr ident)

compileStatements :: [Stmt] -> LabeledInstructions
-- '>>=': Sequentially compose two actions, passing values produced by the first as an argument to the second
compileStatements stmts = mapM compileStmt stmts >>= return . concat
  where
    compileStmt :: Stmt -> LabeledInstructions
    
    compileStmt (Assignment ident exp) = do
      let expCompiled = compileExpression exp
      return ([Lvalue ident] ++ expCompiled ++ [AssignOp])
    
    compileStmt (IfThenElse exp stmt1 stmt2) = do
      elseLabel <- nextLabel
      endLabel <- nextLabel
      let expCompiled = compileExpression exp
      stmt1Compiled <- compileStmt stmt1
      stmt2Compiled <- compileStmt stmt2
      return ( 
                expCompiled
                ++ [GoZero elseLabel]
                ++ stmt1Compiled
                ++ [Go endLabel, Label elseLabel]
                ++ stmt2Compiled
                ++ [Label endLabel]
              )
      
    compileStmt (IfThen exp stmt) = do
      endLabel <- nextLabel
      let expCompiled = compileExpression exp
      stmtCompiled <- compileStmt stmt
      return (
                expCompiled
                ++ stmtCompiled
                ++ [Label endLabel]
              )
    
    compileStmt (While exp stmt) = do
      entryLabel <- nextLabel
      endLabel <- nextLabel
      let expCompiled = compileExpression exp
      stmtCompiled <- compileStmt stmt
      return (
                [Label entryLabel]
                ++ expCompiled
                ++ [GoZero endLabel]
                ++ stmtCompiled
                ++ [Go entryLabel, Label endLabel]
              )
    
    compileStmt (Block stmts) = do
      stmtsCompiled <- compileStatements stmts
      return stmtsCompiled

-- TODO: Compile expressions that are not yet defined in Assembly module      
compileExpression :: Expression -> [Instr]

compileExpression (EXPValue value) = do
  case value of
    STRValue value -> [PushStr value]
    INTValue value -> [PushNat value]

compileExpression (Var ident) = [Rvalue ident]
compileExpression (Add e1 e2) = compileExpression e1 ++ compileExpression e2 ++ [AddOp]
compileExpression (Sub e1 e2) = compileExpression e1 ++ compileExpression e2 ++ [SubOp]
compileExpression (Concat e1 e2) = compileExpression e1 ++ compileExpression e2 ++ [ConcOp]

