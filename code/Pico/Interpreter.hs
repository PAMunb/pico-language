module Pico.Interpreter (
  execute
 ,runProgram
 ,SymbolTable) where

import System.IO

import Pico.AbsPico

-- import Pico.Parser

type Id = String
type SymbolTable = [Decl]

type Environment = [(Id, Value)]

runProgram :: Program -> Value
runProgram (Program st block) =
  let
    env0 = [(v, None) | (Decl (Ident v) t) <- st]  
    env1 = execute (Block block) env0  
    exp = lookup "output" env1  
  in case exp of
    (Just v) -> v
    _        -> error "foo"
     
-- | The interpreter based operational semantics of PICO.
-- It takes a statement and the current environment,
-- and then returns an updated version of the environment.
execute :: Stmt    -- ^ a statement that might change the environment
        -> Environment  -- ^ the current environment
        -> Environment  -- ^ the environment updated after executing the statement.


--
-- in what follows, we present a number of definitions
-- for the execute function... one for each kind of
-- statement. 
--


-- executes an assignment statement. 
execute (Assignment (Ident var) expression) env = (var, val) : [(v,e) | (v, e) <- env, v /= var]
  where val = eval expression env
 
-- executes an IfThenElse statement. 
execute (IfThenElse exp stmtThen stmtElse) env = 
  let condition = eval exp env
  in case condition of
      (INTValue 0) -> execute stmtElse env
      (INTValue _) -> execute stmtThen env
      otherwise   -> error "foo"

-- executes an IfThen statement. 
execute (IfThen exp stmtThen) env =
  let condition = eval exp env
  in case condition of
      (INTValue 0) -> env
      (INTValue _) -> execute stmtThen env 
      otherwise   -> error "foo"

-- executes an While statement. 
execute w@(While exp stmts) env =
  let condition = eval exp env
  in case condition of
      (INTValue 0) -> env 
      (INTValue _) -> let env' = execute stmts env in execute w env'
      otherwise -> error "foo"

-- executes a Block os statements. 
execute (Block []) env   = env
execute (Block (s:ss)) env =
  let env' = execute s env
  in execute (Block ss) env'
      
      
assign :: Ident -> Value -> Environment -> Environment
assign (Ident var) value env = 
 let temp = lookup var env
 in case temp of
     Nothing  -> error "Variable not declared"
     (Just _) -> (var, value) : [(v,e) | (v,e) <- env, v /= var]

-- | The eval function. It evaluates an
--   expression, returning the respective
--   value. 
eval :: Expression -> Environment -> Value

eval (EXPValue value) _ = value

eval (Var (Ident v)) env =
  let exp = lookup v env
  in case exp of
     (Just v) -> v
     (Nothing)-> error $ "Variable " ++ (show v) ++ " not declared"

eval (Add lhs rhs) env = evalBinNatExp lhs rhs (+) env 

eval (Sub lhs rhs) env = evalBinNatExp lhs rhs (-) env 

eval (Mult lhs rhs) env = evalBinNatExp lhs rhs (*) env

eval (Pow lhs rhs) env = evalBinNatExp lhs rhs (^) env

eval (Div lhs rhs) env = evalBinNatExp lhs rhs (quot) env

eval (GTE lhs rhs) env = evalBinMaxExp lhs rhs env

eval (LTE lhs rhs) env = evalBinMinExp lhs rhs env
 
eval (Concat lhs rhs) env = evalBinConcatExp lhs rhs env

-- evalBinExp :: Expression -> Expression -> Environment -> Value
evalBinNatExp lhs rhs op env =
  let (v1, v2) = (eval lhs env, eval rhs env)
  in case (v1, v2) of
     (INTValue n1, INTValue n2) -> INTValue (n1 `op` n2)
     otherwise -> error "Expecting two natural values"

-- evalBinMaxExp :: Expression -> Expression -> Environment -> Value
evalBinMaxExp lhs rhs env = 
  let (v1, v2) = (eval lhs env, eval rhs env)
  in case (v1, v2) of
    (INTValue n1, INTValue n2) -> INTValue (max n1 n2)
    otherwise -> error "Expecting two natural values"

-- evalBinMinExp :: Expression -> Expression -> Environment -> Value
evalBinMinExp lhs rhs env = 
  let (v1, v2) = (eval lhs env, eval rhs env)
  in case (v1, v2) of
    (INTValue n1, INTValue n2) -> INTValue (min n1 n2)
    otherwise -> error "Expecting two natural values"

-- evalBinConcatExp:: Expression -> Expression -> Environment -> Value
evalBinConcatExp lhs rhs env = 
  let (v1, v2) = (eval lhs env, eval rhs env)
  in case (v1, v2) of
    (STRValue s1, STRValue s2) -> STRValue (s1 ++ s2)
    otherwise -> error "Expecting two string values"
