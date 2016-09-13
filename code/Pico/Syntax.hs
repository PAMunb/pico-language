{- |
Module      :  Syntax
Description :  The PICO's abstract syntax 
Copyright   :  (c) Rodrigo Bonifacio
License     :  TO BE DEFINED

Maintainer  :  rbonifacio@unb.br
Stability   :  experimental 
Portability :  portable 

-}

module Pico.Syntax where

import Prelude hiding (GT, LT)

-- | A PICO program is a list of declarations (a symbol table)
-- and a block of statements. 
data Program = Program SymbolTable Block 
 deriving(Show)

type Id = String 

-- | The supported data types of a PICO program
data Type = TNatural | TString 
 deriving(Show)

-- | A declaration is an association between an Id and a Type         
type Declaration = (Id, Type)

-- | A symbol table comprises a set of declarations 
type SymbolTable = [Declaration]

-- | An environment exists during the execution of a
-- PICO program, and models the memory allocated to
-- the program, in terms of variables and their assigned
-- values (expressions). 

type Environment = [(Id, Value)]


-- | PICO supports five types of statements:
-- * assignment
-- * if-then-elese
-- * if-then
-- * while
-- * and a block of statements
data Statement = Assignment Id Expression
               | IfThenElse Expression Statement Statement
               | IfThen Expression Statement
               | While Expression Statement
               | Block [Statement]  
  deriving(Show)

-- | PICO supports five types of expressions:
-- * a value
-- * a reference to a variable
-- * addition involving two natural numbers
-- * subtraction involving two natural numbers
-- * multiplication involving two natural numbers
-- * exponentiation involving two natural numbers
-- * division involving two natural numbers
-- * max number involving two natural numbers
-- * min number involving two natural numbers
-- * and string concatenation
data Expression = ExpValue Value
                | Var Id
                | Add Expression Expression
                | Sub Expression Expression
                | Mult Expression Expression
                | Pow Expression Expression
                | Div Expression Expression
                | GT Expression Expression
                | LT Expression Expression
                | Concat Expression Expression  
  deriving(Show)

-- | PICO supports two types: string values and natural values.
-- However, this implementation also provides supports for
-- types representing an empty value (None) and a type for
-- representing error values. 
data Value = STRValue String
           | NATValue Integer
           | None
           | Error   
  deriving(Eq, Show)

          
type Block = [Statement] 
