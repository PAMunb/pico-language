module Pico.Interpreter
 (execute, runProgram) where

import Pico.Syntax

runProgram :: Program -> Value
runProgram (Program st block) =
  let
    env = execute (Block block) [(v, (ExpValue None)) | (v, _) <- st]
    exp = lookup "output" env  
  in case exp of
      (Nothing) -> error "expecting the declaration of the output variable"
      (Just e)  -> eval e env
     
-- | The interpreter based operational semantics of PICO.
-- It takes a statement and the current environment,
-- and then returns an updated version of the environment.
execute :: Statement    -- ^ a statement that might change the environment
        -> Environment  -- ^ the current environment
        -> Environment  -- ^ the environment updated after executing the statement.


--
-- in what follows, we present a number of definitions
-- for the execute function... one for each kind of
-- statement. 
--


-- executes an assignment statement. 
execute (Assignment var expression) env = assign var expression env

-- executes an IfThenElse statement. 
execute (IfThenElse exp stmtThen stmtElse) env = 
  let condition = eval exp env
  in case condition of
      (NATValue 0) -> execute stmtElse env
      (NATValue _) -> execute stmtThen env
      otherwise   -> error "Invalid expression in IfThenElse statement"

-- executes an IfThen statement. 
execute (IfThen exp stmtThen) env =
  let condition = eval exp env
  in case condition of
      (NATValue 0) -> env
      (NATValue _) -> execute stmtThen env 
      otherwise   -> error "Invalid expression in IfThen statement"

-- executes an While statement. 
execute w@(While exp stmts) env =
  let condition = eval exp env
  in case condition of
      (NATValue 0) -> env 
      (NATValue _) -> let env' = execute stmts env in execute w env'
      otherwise -> error "Invalid expression in While statement" 

-- executes a Block os statements. 
execute (Block []) env   = env
execute (Block (s:ss)) env =
  let env' = execute s env
  in execute (Block ss) env'
      

assign :: Id -> Expression -> Environment -> Environment
assign var exp env = 
 let temp = lookup var env
 in case temp of
     Nothing  -> error "Variable not declared"
     (Just _) -> (var, exp) : [(v,e) | (v,e) <- env, v /= var]

-- | The eval function. It evaluates an
--   expression, returning the respective
--   value. 
eval :: Expression -> Environment -> Value

eval (ExpValue value) _ = value

eval (Var v) env =
  let exp = lookup v env
  in case exp of
     (Just e) -> eval e env
     (Nothing)-> error $ "Variable " ++ (show v) ++ " not declared"

eval (Add lhs rhs) env =
  let (v1, v2) = (eval lhs env, eval rhs env)
  in case (v1, v2) of
     (NATValue n1, NATValue n2) -> NATValue (n1 + n2)
     otherwise -> error "Expecting two natural values" 

-- lookup :: Id -> Environment -> Maybe Expression
-- lookup var env =
--   let res = [(v, e) | (v, e) <- env, v == var]
--   in case res of
--      [(v,e)]   -> Just e
--      otherwise -> Nothing


  
