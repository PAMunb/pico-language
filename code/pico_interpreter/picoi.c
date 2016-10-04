// C99 compliant assembly interpreter
// Original author: Rafael Diniz

/*******************************************************/
/***************** Intruction SET **********************/
/*

TODO

*/

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
            fprintf(stderr, "picoi syntax:\n");
            fprintf(stderr, "\tpicoi [-v] [-i input.asm]\n");
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
