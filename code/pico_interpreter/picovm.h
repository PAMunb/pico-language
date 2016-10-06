// C99 compliant pico assembly Virtual Machine.
// Original author: Rafael Diniz

#ifndef PICOVM_H
#define PICOVM_H

/**************************************************************************/
/***************** Intruction SET *****************************************/

// No Operation
#define NOOP 0

// Declare Integer (MEM)
// dclNat("Var_Name")
#define DCLNAT 1

// Declare String (MEM)
// dclStr("Var_Name")
#define DCLSTR 2

// Push Integer to stack (stack++)
// pushNat(Int)
#define PUSHNAT 3

// Push String to stack (*stack++)
// pushStr("String")
#define PUSHSTR 4

// Push the value of a variable to stack (stack++)
// rvalue("Var_Name")
#define RVALUE 5

// Push the address of a variable to stack (stack++)
// lvalue("Var_Name")
#define LVALUE 6

// Assigns value on top of the stack to variable at address top-1 (stack-2)
// assign()
#define ASSIGN 7

// Sum top 2 stack values of stack and keep the sum (stack--)
// add2()
#define ADD2 8

// Subtraction of top 2 slack values (stack--)
// sub2()
#define SUB2 9

// Concatenates top 2 stack values (stack--)
// conc2()
#define CONC2 10

// Label to next instruction
// label("Label")
#define LABEL 11

// Inconditional GOTO
// go("Label")
#define GO 12

// Jump If Zero (stack--)
// gotrue("Label")
#define GOTRUE 13

// Jump If Not Zero (stack--)
// gofalse("Label")
#define GOFALSE 14
/**************************************************************************/

#define MAX_NAME_SIZE 256

// Instruction arguments
union args {
    int constant;
    char var_name[MAX_NAME_SIZE];
    char label[MAX_NAME_SIZE];
    char string[MAX_NAME_SIZE];
};

// Our instruction structure
typedef struct {
    int instr_id;
    union args arg;
} op;

// Our memory unit for integers and string. The structure for the memory
// is a linked list, with mem_head being the head.
typedef struct memory_int{
    char id[MAX_NAME_SIZE];
    int64_t val;
    char *str;
    struct memory_int *next;
} mem_unit;

// the stack structure, also a linked list, linked in a top-down way.
typedef struct stack_str{
    int64_t val;
    char *str;
    struct stack_str *previous;
} stack_unit;

#endif // PICOVM_H
