module Pico.Assembly where

-- Ident should be imported from AbsPico?
import Pico.AbsPico

type Label = String

data Instr
    = DclInt Ident -- Reserve a memory location for an integer variable
    | DclStr Ident -- Reserve a memory location for a string variable
    | PushNat Integer -- Push integer constant on the stack
    | PushStr String -- Push string constant on the stack
    | Rvalue Ident -- Push the value of a variable on the stack
    | Lvalue Ident -- Push the address of a variable on the stack
    | AssignOp -- Assign value on top, to variable at address top-1
    | AddOp -- Replace top two stack values by their sum
    | SubOp -- Replace top two stack values by their difference
    | PowOp -- Replace top stack value by power
    | MultOp -- Replace top two stack values by their multiplication
    | DivOp -- Replace top two stack values by their division
    | GTEOp -- Replace top two stack values by greater than or equal operation
    | LTEOp -- Replace top two stack values by less than or equal operation
    | ConcOp -- Replace top two stack values by their concatenation
    | Label Label -- Associate a label with the next instruction
    | Go Label -- Go to instruction with given label
    | GoZero Label -- Go to instruction with given label, if top equals 0
    | GoNonZero Label -- Go to instruction with given label, if top not equal to 0
     deriving (Eq, Show, Read)