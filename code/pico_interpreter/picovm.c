// C99 compliant pico assembly Virtual Machine.
// Original author: Rafael Diniz

// C headers
#include <unistd.h>
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>

#include "picovm.h"

// Global variables //
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
// enable interactive mode
bool interactive = false;

// Instruction handlers
int run_noop(op instr){
    // No Operation
    return ip;
}

int run_dclnat(op instr){
    mem_unit *temp;
    mem_unit *var;

    if(verbose){
        fprintf(stderr, "dclNat %s\n", instr.arg.var_name);
    }

    var = (mem_unit *) malloc (sizeof(mem_unit));
    var->val = 0;
    var->str = NULL;
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
    mem_unit *temp;
    mem_unit *var;

    if(verbose){
        fprintf(stderr, "dclStr %s\n", instr.arg.var_name);
    }

    var = (mem_unit *) malloc (sizeof(mem_unit));
    var->val = 0;
    var->str = NULL;
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
    stack_unit *stack = (stack_unit *) malloc(sizeof(stack_unit));

    if (verbose){
        fprintf(stderr, "pushStr %s\n", instr.arg.string);
    }

    stack->str = instr.arg.string;

    if (stack_top == NULL){
        stack->previous = NULL;
        stack_top = stack;
    } else {
        stack->previous = stack_top;
        stack_top = stack;
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
        var = var->next;
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
        var = var->next;
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
    stack_unit *stack = stack_top->previous;
    mem_unit *var = mem_head;

    if (verbose){
        fprintf(stderr, "assign\n");
    }

    // find the address of the variable in the memory
    for (int i = 0; i < stack->val; i++){
        var = var->next;
    }

    // assign value to variable
    var->val = stack_top->val;

    // clean up stack
    free(stack_top);
    stack_top = stack;
    stack = stack_top->previous;
    free(stack_top);
    stack_top = stack;

    return ip+1;
}

int run_add2(op instr){
    stack_unit *stack = stack_top->previous;

    if (verbose){
        fprintf(stderr, "add2 %"PRId64" %"PRId64"\n", stack_top->val, stack->val);
    }

    // sum
    stack->val = stack->val + stack_top->val;

    // clean up stack
    free(stack_top);
    stack_top = stack;

    return ip+1;
}

int run_sub2(op instr){
    stack_unit *stack = stack_top->previous;

    if (verbose){
        fprintf(stderr, "sub2 %"PRId64" %"PRId64"\n", stack_top->val, stack->val);
    }

    // subtraction
    stack->val = stack->val - stack_top->val;;

    // clean up stack
    free(stack_top);
    stack_top = stack;

    return ip+1;
}

int run_conc2(op instr){

    if (verbose){
        fprintf(stderr, "conc2 Not Implemented Yet\n");
    }

    return ip+1;
}

int run_go(op instr){
    mem_unit *label = labels_head;

    if (verbose){
        fprintf(stderr, "go %s\n", instr.arg.label);
    }

    if (label == NULL){
    bail:
        fprintf(stderr, "Error executing go()\n");
        return ip+1;
    }


    while (strcmp(label->id, instr.arg.label)){
        if (label->next == NULL){
            goto bail;
        }
        label = label->next;
    }
    return label->val;
}

int run_gotrue(op instr){
    mem_unit *label = labels_head;
    stack_unit *stack = stack_top->previous;
    int ip_next = 0;

    if (verbose){
        fprintf(stderr, "gotrue %s\n", instr.arg.label);
    }

    if (label == NULL){
    bail:
        fprintf(stderr, "Error executing gotrue()\n");
        return ip+1;
    }

    // if stack is 0
//    if (stack_top->val == 0){
    if (stack_top->val != 0){
        while (strcmp(label->id, instr.arg.label)){
            if (label->next == NULL){
                goto bail;
            }
            label = label->next;
        }
        ip_next = label->val;
    } else {
        ip_next = ip + 1;
    }

    free(stack_top);
    stack_top = stack;

    return ip_next;
}

int run_gofalse(op instr){
    mem_unit *label = labels_head;
    stack_unit *stack = stack_top->previous;
    int ip_next = 0;

    if (verbose){
        fprintf(stderr, "gofalse %s\n", instr.arg.label);
    }

    if (label == NULL){
    bail:
        fprintf(stderr, "Error executing gofalse()\n");
        return ip+1;
    }

    // if stack is != 0
//    if (stack_top->val != 0){
    if (stack_top->val == 0){
        while (strcmp(label->id, instr.arg.label)){
            if (label->next == NULL){
                goto bail;
            }
            label = label->next;
        }
        ip_next = label->val;
    } else {
        ip_next = ip + 1;
    }

    free(stack_top);
    stack_top = stack;

    return ip_next;
}

// adds instruction to the executable (text) area of the memory.
bool stack_instruction(op instr){
    static int instr_nr = 0;
    static op last_label;
    static bool latest_is_label = false;

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

// Gets next instruction from input
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

    fprintf(stderr, "=====================================================================\n");
    fprintf(stderr, "picovm 0.1\nIf you don't know how to use picovm, type %s -h for help.\n", argv[0]);
    fprintf(stderr, "=====================================================================\n\n");
// Command line parsing
    int c;
    while ((c = getopt (argc, argv, "gvi:")) != -1){
        switch (c)
        {
        case 'g':
            interactive = true;
            break;
        case 'v':
            verbose = true;
            break;
        case 'i':
            file_name = optarg;
            break;
        default:
        bail:
            fprintf(stderr, "picovm syntax:\n");
            fprintf(stderr, "\tpicovm [-v] [-g] [-i input.asm]\n");
            fprintf(stderr, "\t-v              Enables verbose.\n");
            fprintf(stderr, "\t-g              Enables interactive mode (use with -i).\n");
            fprintf(stderr, "\t-i file_name... Execute instructions from file, instead of stdin.\n");
            return EXIT_FAILURE;
        }
    }

    if (interactive && !file_name)
        goto bail;
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

        // here we allow stacking the NOOP as last instruction in order to
        // allow label() as last command in input assembly.
        stack_instruction(instr);
        if (instr.instr_id == NOOP) {
            run = false;
            continue;
        }
    }

    // prints the executable area
    if (verbose){
        for (int i = 0; i < instruction_count; i++){
            fprintf(stderr,"OP: %d\n", text[i].instr_id);
        }
        fprintf(stderr, "\n");
    }

    // Run the code
    run = true;
    while(run){
        if (interactive){
            fprintf(stderr, "Press any key to execute the next instruction...\n");
            getchar();            getchar();
        }
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

        if (verbose){
            stack_unit *stk;
            // prints the stack
            stk = stack_top;
            fprintf(stderr, "IP: %d\n", ip);
            while (stk){
                fprintf(stderr, "stack val: %"PRId64" string: %s\n", stk->val, stk->str);
                stk = stk->previous;
            }
            // prints the memory
            mem_unit *temp;
            temp = mem_head;
            while (temp){
                fprintf(stderr, "memory id: %s val: %"PRId64"\n", temp->id, temp->val);
                temp = temp->next;
            }
            // prints the labels list
            temp = labels_head;
            while (temp){
                fprintf(stderr, "label id: %s val: %"PRId64" instr: %d\n", temp->id, temp->val, text[temp->val].instr_id);
                temp = temp->next;
            }
        }

    }
    // print the variable output
    mem_unit *var = mem_head;
    // find the variable in the memory
    if (var == NULL){
        fprintf(stderr, "No output variable declared.\n");
    } else {
        while (strcmp(var->id, "output")){
            if (var->next == NULL){
                fprintf(stderr, "No output variable declared.\n");
                break;
            }
            var = var->next;
        }
        fprintf(stderr, "output = %"PRId64"\n", var->val);
    }


    // Deallocate memory



    if (fin)
        fclose(fin);

    return EXIT_SUCCESS;
}
