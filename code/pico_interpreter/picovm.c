// C99 compliant pico intermediate assembly Virtual Machine.
// Original author: Rafael Diniz

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

// C headers
#include <unistd.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

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

// Our memory unit for integers, the structure for the memory
// is a linked list, with mem_head being the head.
typedef struct memory_int{
    char id[MAX_NAME_SIZE];
    int val;
    struct memory_int *next;
} mem_unit;

// the stack structure, also a linked list, linked in a top-down way.
typedef struct stack_str{
    int val;
    char *str;
    struct stack_str *previous;
} stack_unit;

// Global variables
// instruction pointer
int ip = 0;
// stack
stack_unit *stack_top = NULL;
// memory
mem_unit *mem_head = NULL;
// labels
mem_unit *labels_head = NULL;
// text area (the instructions loaded in memory)
op *text;
// enable verbose output
bool verbose = false;

int run_noop(op instr){
    // No Operation
    return ip+1;
}

int run_dclnat(op instr){
    mem_unit *temp;
    mem_unit *var;

    if(verbose){
        fprintf(stderr, "dclNat %s\n", instr.arg.var_name);
    }

    var = (mem_unit *) malloc (sizeof(mem_unit));
    var->val = 0;
    strcpy (var->id, instr.arg.var_name);
    var->next = NULL;

    if (mem_head == NULL) {
        mem_head = var;
    } else {
        temp = mem_head;
        while (temp->next != NULL)
            temp = temp->next;
        temp->next = var;
    }

    return ip+1;
}

int run_dclstr(op instr){

    if (verbose){
        fprintf(stderr, "dclStr Not Implemented Yet\n");
    }

    return ip+1;
}

int run_pushnat(op instr){
    stack_unit *stack = (stack_unit *) malloc(sizeof(stack_unit));

    if (verbose){
        fprintf(stderr, "pushNat %d\n", instr.arg.constant);
    }

    stack->val = instr.arg.constant;

    if (stack_top == NULL){
        stack->previous = NULL;
        stack_top = stack;
    } else {
        stack->previous = stack_top;
        stack_top = stack;
    }

    return ip+1;
}

int run_pushstr(op instr){

    if (verbose){
        fprintf(stderr, "pushStr Not Implemented Yet\n");
    }

    return ip+1;
}

int run_rvalue(op instr){
    mem_unit *var = mem_head;

    if (verbose){
        fprintf(stderr, "rvalue %s\n", instr.arg.var_name);
    }

    if (var == NULL){
    bail:
        fprintf(stderr, "Error executing rvalue()\n");
        return ip+1;
    }

    // find the variable in the memory
    while (strcmp(var->id, instr.arg.var_name)){
        if (var->next == NULL){
            goto bail;
        }
    }

    // put it in stack
    stack_unit *stack = (stack_unit *) malloc(sizeof(stack_unit));

    stack->val = var->val;

    if (stack_top == NULL){
        stack->previous = NULL;
        stack_top = stack;
    } else {
        stack->previous = stack_top;
        stack_top = stack;
    }

    return ip+1;
}

int run_lvalue(op instr){
    mem_unit *var = mem_head;

    if (verbose){
        fprintf(stderr, "lvalue %s\n", instr.arg.var_name);
    }

    if (var == NULL){
    bail:
        fprintf(stderr, "Error executing lvalue()\n");
        return ip+1;
    }

    // find the address of the variable in the memory
    int i = 0;
    while (strcmp(var->id, instr.arg.var_name)){
        i++;
        if (var->next == NULL){
            goto bail;
        }
    }

    // put it in stack
    stack_unit *stack = (stack_unit *) malloc(sizeof(stack_unit));

    stack->val = i;

    if (stack_top == NULL){
        stack->previous = NULL;
        stack_top = stack;
    } else {
        stack->previous = stack_top;
        stack_top = stack;
    }

    return ip+1;
}

int run_assign(op instr){

    if (verbose){
        fprintf(stderr, "assign Not Implemented Yet\n");
    }

    return ip+1;
}

int run_add2(op instr){

    if (verbose){
        fprintf(stderr, "add2 Not Implemented Yet\n");
    }

    return ip+1;
}

int run_sub2(op instr){

    if (verbose){
        fprintf(stderr, "sub2 Not Implemented Yet\n");
    }

    return ip+1;
}

int run_conc2(op instr){

    if (verbose){
        fprintf(stderr, "conc2 Not Implemented Yet\n");
    }

    return ip+1;
}

int run_go(op instr){

    if (verbose){
        fprintf(stderr, "go Not Implemented Yet\n");
    }

    return ip+1;
}

int run_gotrue(op instr){

    if (verbose){
        fprintf(stderr, "gotrue Not Implemented Yet\n");
    }

    return ip+1;
}

int run_gofalse(op instr){

    if (verbose){
        fprintf(stderr, "gofalse Not Implemented Yet\n");
    }

    return ip+1;
}

bool stack_instruction(op instr){
    static int instr_nr = 0;
    static op last_label;
    static bool latest_is_label = false;

    printf("instr: %d\n", instr.instr_id);

    if (instr.instr_id == LABEL){
        latest_is_label = true;
        last_label = instr;
        return true;
    }

    if (latest_is_label) {
        mem_unit *label = (mem_unit *) malloc(sizeof(mem_unit));
        mem_unit *temp;

        strcpy(label->id, last_label.arg.label);
        label->val = instr_nr;
        label->next = NULL;
        if (labels_head == NULL){
            labels_head = label;
        } else {
            temp = labels_head;
            while (temp->next != NULL)
                temp = temp->next;
            temp->next = label;
        }
        latest_is_label = false;
    }

    text[instr_nr] = instr;
    instr_nr++;

    return true;
}

op get_instruction(FILE *fin) {
    char instruction[1024];
    op instr;

    // the format of the instructions in the assembly file are comma
    // separated, the last instruction without comma. Eg.
    //
    // pushNat(10),
    // pushNat(5),
    // sub2()
    //
    // No comments or nothing more allowed in the file,
    // the parser is ad-hoc, and follows...
    instr.instr_id = NOOP;

    int ret = fscanf(fin, "%[^(]", instruction);

    if (ret == EOF){
        return instr;
    }

    if (verbose)
        fprintf(stderr, "OP: %s\n", instruction);

    if (strstr(instruction, "dclNat"))
        instr.instr_id = DCLNAT;

    if (strstr(instruction, "dclStr"))
        instr.instr_id = DCLSTR;

    if (strstr(instruction, "pushNat"))
        instr.instr_id = PUSHNAT;

    if (strstr(instruction, "pushStr"))
        instr.instr_id = PUSHSTR;

    if (strstr(instruction, "rvalue"))
        instr.instr_id = RVALUE;

    if (strstr(instruction, "lvalue"))
        instr.instr_id = LVALUE;

    if (strstr(instruction, "assign"))
        instr.instr_id = ASSIGN;

    if (strstr(instruction, "add2"))
        instr.instr_id = ADD2;

    if (strstr(instruction, "sub2"))
        instr.instr_id = SUB2;

    if (strstr(instruction, "conc2"))
        instr.instr_id = CONC2;

    if (strstr(instruction, "label"))
        instr.instr_id = LABEL;

    if (strstr(instruction, "go"))
        instr.instr_id = GO;

    if (strstr(instruction, "gotrue"))
        instr.instr_id = GOTRUE;

    if (strstr(instruction, "gofalse"))
        instr.instr_id = GOFALSE;

    if (instr.instr_id == DCLNAT ||
        instr.instr_id == DCLSTR ||
        instr.instr_id == RVALUE ||
        instr.instr_id == LVALUE){

        fscanf(fin, "%[^\"]\"", instruction);
        fscanf(fin, "%[^\"]\")", instruction);
        strncpy (instr.arg.var_name, instruction, MAX_NAME_SIZE);

        if (verbose)
            fprintf(stderr, "var_name: %s\n", instr.arg.var_name);
    }

    if (instr.instr_id == PUSHNAT){
        fscanf(fin, "%[(]", instruction);
        fscanf(fin, "%[^)])", instruction);
        instr.arg.constant = atoi(instruction);

        if (verbose)
            fprintf(stderr, "constant: %d\n", instr.arg.constant);
    }

    if (instr.instr_id == PUSHSTR){
        fscanf(fin, "%[^\"]\"", instruction);
        fscanf(fin, "%[^\"]\")", instruction);

        strncpy (instr.arg.string, instruction, MAX_NAME_SIZE);

        if (verbose)
            fprintf(stderr, "string: %s\n", instr.arg.string);
    }

    if (instr.instr_id == GO ||
        instr.instr_id == GOTRUE ||
        instr.instr_id == GOFALSE ||
        instr.instr_id == LABEL){

        fscanf(fin, "%[^\"]\"", instruction);
        fscanf(fin, "%[^\"]\")", instruction);
        strncpy (instr.arg.label, instruction, MAX_NAME_SIZE);

        if (verbose)
            fprintf(stderr, "label_name: %s\n", instr.arg.var_name);
    }

    fscanf(fin, "%[^\n]\n", instruction);

    return instr;
}

int main(int argc, char **argv) {
    char *file_name = NULL;
    FILE *fin = NULL;
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

    // Gets the size of the executable
    int instruction_count = 0;
    while (run) {
        op instr = get_instruction(fin);

        if (instr.instr_id == NOOP) {
            run = false;
            continue;
        }

        if (instr.instr_id == LABEL) {
            continue;
        }
        instruction_count++;
    }

    // Going back to the begining of the file
    rewind(fin);

    if (verbose)
        fprintf(stderr, "Number of instructions (labels not counted): %d\n",
                instruction_count);

    // Allocate space for the executable + NOOP at the end
    instruction_count++;
    text = (op *) calloc (instruction_count, sizeof(op));

    // Load assembly code to memory
    run = true;
    while (run) {
        op instr = get_instruction(fin);

        // here we allow stacking the NOOP in order to
        // allow labeling the last instruction.
        stack_instruction(instr);

        if (instr.instr_id == NOOP) {
            run = false;
            continue;
        }
    }

#if 1
    if (verbose) {
        // prints the executable area
        for (int i = 0; i < instruction_count; i++){
            fprintf(stderr,"OP: %d\n", text[i].instr_id);
        }

        fprintf(stderr, "\n");
        mem_unit *temp;

        // prints the memory
        temp = mem_head;
        while (temp){
            fprintf(stderr, "memory id: %s val: %d\n", temp->id, temp->val);
            temp = temp->next;
        }
        // prints the labels list
        temp = labels_head;
        while (temp){
            fprintf(stderr, "label id: %s val: %d instr: %d\n", temp->id, temp->val, text[temp->val].instr_id);
            temp = temp->next;
        }
    }
#endif

    // Run the code
    run = true;
    while(run){

        switch(text[ip].instr_id){
        case NOOP:
            ip = run_noop(text[ip]);
            run = false;
            break;
        case DCLNAT:
            ip = run_dclnat(text[ip]);
            break;
        case DCLSTR:
            ip = run_dclstr(text[ip]);
            break;
        case PUSHNAT:
            ip = run_pushnat(text[ip]);
            break;
        case PUSHSTR:
            ip = run_pushstr(text[ip]);
            break;
        case RVALUE:
            ip = run_rvalue(text[ip]);
            break;
        case LVALUE:
            ip = run_lvalue(text[ip]);
            break;
        case ASSIGN:
            ip = run_assign(text[ip]);
            break;
        case ADD2:
            ip = run_add2(text[ip]);
            break;
        case SUB2:
            ip = run_sub2(text[ip]);
            break;
        case CONC2:
            ip = run_conc2(text[ip]);
            break;
        case GO:
            ip = run_go(text[ip]);
            break;
        case GOTRUE:
            ip = run_gotrue(text[ip]);
            break;
        case GOFALSE:
            ip = run_gofalse(text[ip]);
            break;
        default:
            fprintf(stderr, "Internal error.\n");
        }

    }


    // Deallocate memory

    // output the variable output

    if (fin)
        fclose(fin);

    return EXIT_SUCCESS;
}
