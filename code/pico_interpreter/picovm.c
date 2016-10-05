// C99 compliant pico intermediate assembly Virtual Machine.
// Original author: Rafael Diniz

/**************************************************************************/
/***************** Intruction SET *****************************************/

// Declare Integer (MEM)
// DclNat("Var_Name")
#define DCLNAT 1

// Declare String (MEM)
// DclStr("Var_Name")
#define DCLSTR 2

// Push Integer to stack (stack++)
// PushNat(Int)
#define PUSHNAT 3

// Push String to stack (*stack++)
// PushStr("String")
#define PUSHSTR 4

// Push the value of a variable to stack (stack++)
// Rvalue("Var_Name")
#define RVALUE 5

// Push the address of a variable to stack (stack++)
// Lvalue("Var_Name")
#define LVALUE 6

// Assigns value on top of the stack to variable at address top-1 (stack-2)
// Assign()
#define ASSIGN 7

// Sum top 2 stack values of stack and keep the sum (stack--)
// Add2()
#define ADD2 8

// Subtraction of top 2 slack values (stack--)
// Sub2()
#define SUB2 9

// Concatenates top 2 stack values (stack--)
// Conc2()
#define CONC2 10

// Label to next instruction
// Label("Label")
#define LABEL 11

// Inconditional GOTO
// Go("Label")
#define GO 12

// Jump If Zero (stack--)
// GoTrue("Label")
#define GOTRUE 13

// Jump If Not Zero (stack--)
// GoFalse("Label")
#define GONONZERO 14
/**************************************************************************/

#include <unistd.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

int main(int argc, char **argv) {
    int ip = 0;
    char *file_name = NULL;
    FILE *fin = NULL;
    bool verbose = false;
    bool run = true;

// Command line parsing
    int c;
    while ((c = getopt (argc, argv, "vi:")) != -1){
        switch (c)
        {
        case 'v':
            verbose = true;
            break;
        case 'i':
            file_name = optarg;
            break;
        default:
            fprintf(stderr, "picovm syntax:\n");
            fprintf(stderr, "\tpicovm [-v] [-i input.asm]\n");
            fprintf(stderr, "\t-v              Enables verbose.\n");
            fprintf(stderr, "\t-i file_name... Execute instructions from file, instead of stdin.\n");
            return EXIT_FAILURE;
        }
    }

    fin = (file_name)? fopen(file_name, "r") : stdin;

    while (run) {
        // get a line
        // fscanf(fin, "...",...);

        // exit loop
        run = false;
    }

    if (fin)
        fclose(fin);

    return EXIT_SUCCESS;
}
