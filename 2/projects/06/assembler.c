#include <stdio.h>
#include "table.h"
#include "string.h"
#include "handlelines.h"

FILE *fp;
FILE *fopen(const char *name, const char *mode);

int main(int argc, char *argv[])
{
    char **commands;
    char *line;

    if (argc != 2)
        printf("%s\n", "Assembler takes one argument.");
    else
        if((fp = fopen(*++argv, "r")) == NULL){
            printf("Assembler: can't open %s\n", *argv);
            return 1;
        }else {
            t = make_table(number_of_lines(fp));
            commands = getlines(fp);
            printf("%d", number_of_lines(fp));
            table *t;
            insert("a", "poop", t);
        }
}

