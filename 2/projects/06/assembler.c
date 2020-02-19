#include "symboltable.h"

FILE *fp;

int main(int argc, char *argv[])
{
    char **commands;
    char *line;
    table *t;

    if (argc != 2)
        printf("%s\n", "Assembler takes one argument.");
    else
        if((fp = fopen(*++argv, "r")) == NULL){
            printf("Assembler: can't open %s\n", *argv);
            return 1;
        }else {
            t = make_table(number_of_lines(fp)); 
            commands = getlines(fp);
            build_symbol_table(commands, t);
        }
}

