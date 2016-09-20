module Pico.TypeChecker (typeCheck) where

import Pico.AbsPico 

type SymbolTable = [Decl]

typeCheck :: Program -> Bool
typeCheck (Program st stmts) = and $ map (\s -> checkStatement s st) stmts 

checkStatement :: Stmt -> SymbolTable -> Bool
checkStatement (Assignment var exp) st = t1 == t2
 where
   t1 = getSymbolType var  st
   t2 = getExpType exp st

checkStatement (IfThenElse exp stm1 stm2) st = t1 == TInteger && checkStatement stm1 st && checkStatement stm2 st
 where
   t1 = getExpType exp st

checkStatement (IfThen exp stm1) st = t1 == TInteger && checkStatement stm1 st
 where
   t1 = getExpType exp st

checkStatement (While exp stm1) st = t1 == TInteger && checkStatement stm1 st
 where
   t1 = getExpType exp st

checkStatement (Block stmts) st = and $ map (\s -> checkStatement s st) stmts 


-- | verifies if a expression is well-typed or not
checkExp :: Expression -> SymbolTable -> Bool

checkExp (EXPValue _) st = True 

checkExp (Var id) st = True   -- still not sure about this.    

checkExp (Add exp1 exp2) st = checkNaturalExp exp1 exp2 st

checkExp (Sub exp1 exp2) st = checkNaturalExp exp1 exp2 st

checkExp (Mult exp1 exp2) st = checkNaturalExp exp1 exp2 st

checkExp (Div exp1 exp2) st = checkNaturalExp exp1 exp2 st

checkExp (Pow exp1 exp2) st = checkNaturalExp exp1 exp2 st

checkExp (Concat exp1 exp2) st =
 let
    t1 = getExpType exp1 st
    t2 = getExpType exp2 st
 in 
  case (t1, t2) of
    (TString, TString) -> True
    _ -> False

-- just an auxiliarly function to deal with
-- binary natural expressions.
checkNaturalExp exp1 exp2 st =
 let
  t1 = getExpType exp1 st
  t2 = getExpType exp2 st
 in case (t1, t2) of
  (TInteger, TInteger) -> True
  _                    -> False


-- | The type of a given variable declared in the symbol table. 
getSymbolType ::  Ident -> SymbolTable -> Type
getSymbolType var st =
  let res = [t |(Decl v t) <- st, v == var]
  in case res of
     [t] -> t
     otherwise -> error "variable  not declared" 

-- | The type of an expression.      
getExpType :: Expression -> SymbolTable -> Type

getExpType (EXPValue x) st = 
  case x of
    INTValue x -> TInteger
    STRValue x -> TString
    otherwise -> error "foo"
   
getExpType (Var var) st =  head [t |(Decl v t) <- st, v == var]

getExpType (Add lhs rhs) st = getTypeBinaryExp lhs rhs st TInteger

getExpType (Sub lhs rhs) st = getTypeBinaryExp lhs rhs st TInteger

getExpType (Mult lhs rhs) st = getTypeBinaryExp lhs rhs st TInteger

getExpType (Pow lhs rhs) st = getTypeBinaryExp lhs rhs st TInteger

getExpType (Div lhs rhs) st = getTypeBinaryExp lhs rhs st TInteger

getExpType (Concat lhs rhs) st = getTypeBinaryExp lhs rhs st TString

getTypeBinaryExp :: Expression -> Expression -> SymbolTable -> Type -> Type
getTypeBinaryExp lhs rhs st t=
  let
    t1 = getExpType lhs st
    t2 = getExpType rhs st
   in if t1 == t2 && t1 == t then t else error "foo"
