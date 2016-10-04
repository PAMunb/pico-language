module Pico.Assembly where

type Label = String
type Name = String

-- TODO: define instructions for expressions:
---- GTE
---- LTE
---- Mult
---- Div
---- Pow

data Instr
    = DclInt Name -- Reserve a memory location for an integer variable
    | DclStr Name -- Reserve a memory location for a string variable
    | PushNat Int -- Push integer constant on the stack
    | PushStr String -- Push string constant on the stack
    | Rvalue Name -- Push the value of a variable on the stack
    | Lvalue Name -- Push the address of a variable on the stack
    | AssignOp -- Assign value on top, to variable at address top-1
    | AddOp -- Replace top two stack values by their sum
    | SubOp -- Replace top two stack values by their difference
    | ConcOp -- Replace top two stack values by their concatenation
    | Label Label -- Associate a label with the next instruction
    | Go Label -- Go to instruction with given label
    | GoZero Label -- Go to instruction with given label, if top equals 0
    | GoNonZero Label -- Go to instruction with given label, if top not equal to 0
     deriving (Eq, Show, Read)