module Pico.TypeChecker where

import Pico.Syntax

typeCheck :: Program -> Bool
typeCheck (Program st (b:bs)) = (checkStatement st b) && (checkStatement st (Block bs))

checkStatement :: SymbolTable -> Statement -> Bool
checkStatement st (Assignment id exp) = if checkEqualTypes t1 t2
	then True && (checkExp st exp)
	else False
	where 
		t1 = getTypeId st id
		t2 = getTypeExp st exp

checkStatement st (IfThenElse exp stm1 stm2) = (checkEqualTypes t1 TNatural)
	&& checkStatement st stm1
	&& checkStatement st stm2
	where
		t1 = getTypeExp st exp

checkStatement st (IfThen exp stm1) = (checkEqualTypes t1 TNatural)
	&& checkStatement st stm1
	where
		t1 = getTypeExp st exp

checkStatement st (While exp stm1) = (checkEqualTypes t1 TNatural)
	&& checkStatement st stm1
	where
		t1 = getTypeExp st exp

checkStatement st (Block (s:ss)) = checkStatement st s && checkStatement st (Block ss)

checkExp :: SymbolTable -> Expression -> Bool
checkExp st (ExpValue exp1) = True -- doubt

checkExp st (Var id) = 			   -- doubt
	case (texp1) of
		TNatural -> True
		TString -> True
		_ -> False
	where
		texp1 = getTypeId st id

checkExp st (Add exp1 exp2) =
	case (texp1, texp2) of
		(TNatural, TNatural) -> True
		_ -> False
	where
		texp1 = getTypeExp st exp1
		texp2 = getTypeExp st exp2

checkExp st (Sub exp1 exp2) =
	case (texp1, texp2) of
		(TNatural, TNatural) -> True
		_ -> False
	where
		texp1 = getTypeExp st exp1
		texp2 = getTypeExp st exp2

checkExp st (Mult exp1 exp2) =
	case (texp1, texp2) of
		(TNatural, TNatural) -> True
		_ -> False
	where
		texp1 = getTypeExp st exp1
		texp2 = getTypeExp st exp2

checkExp st (Div exp1 exp2) =
	case (texp1, texp2) of
		(TNatural, TNatural) -> True
		_ -> False
	where
		texp1 = getTypeExp st exp1
		texp2 = getTypeExp st exp2

checkExp st (Pow exp1 exp2) =
	case (texp1, texp2) of
		(TNatural, TNatural) -> True
		_ -> False
	where
		texp1 = getTypeExp st exp1
		texp2 = getTypeExp st exp2

checkExp st (Concat exp1 exp2) =
	case (texp1, texp2) of
		(TString, TString) -> True
		_ -> False
	where
		texp1 = getTypeExp st exp1
		texp2 = getTypeExp st exp2

checkEqualTypes :: Type -> Type -> Bool
checkEqualTypes type1 type2 
	| type1 == type2 = True
	| otherwise = False

getTypeId :: [Declaration] -> Id -> Type
getTypeId st id = head [xType |(xId, xType) <- st, xId == id]

getTypeExp :: [Declaration] -> Expression -> Type
-- Doubt, if none value and error what type return
getTypeExp _ (ExpValue x) = 
	case x of
		NATValue x -> TNatural
		STRValue x -> TString
--		None -> none     -- doubt
--		_ -> error       -- doubt

getTypeExp st (Var v) =  head [vType |(vId, vType) <- st, vId == v]
getTypeExp st (Add lhs rhs) = getTypeBinExp st lhs rhs
getTypeExp st (Sub lhs rhs) = getTypeBinExp st lhs rhs
getTypeExp st (Mult lhs rhs) = getTypeBinExp st lhs rhs
getTypeExp st (Pow lhs rhs) = getTypeBinExp st lhs rhs
getTypeExp st (Div lhs rhs) = getTypeBinExp st lhs rhs
getTypeExp st (Concat lhs rhs) = getTypeBinExp st lhs rhs

getTypeBinExp :: [Declaration] -> Expression -> Expression -> Type
getTypeBinExp st lhs rhs =
	case (v1, v2) of
		(TNatural, TNatural) -> TNatural
		(TString, TString) -> TString
		-- None and Error?
		otherwise -> error "TYPE ERROR"
	where
		v1 = getTypeExp st lhs
		v2 = getTypeExp st rhs
