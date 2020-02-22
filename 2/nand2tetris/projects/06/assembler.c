#include "symboltable.h"
#include "parser.h"
#include "code.h"

FILE *fp;
FILE *fo;

char *output_filename(char *name){
    int i, cursor;

    for(i=0; name[i] != '\0'; i++)
        if( name[i] == '/' )
            cursor = i;
    name = name + cursor + 1;

    for(i=0; name[i] != '\0'; i++)
        if( name[i] == '.' )
            cursor = i;
    cursor++;

    name[cursor] = 'h';
    name[cursor+1] = 'a';
    name[cursor+2] = 'c';
    name[cursor+3] = 'k';
    name[cursor+4] = '\0';

    return name;

}

void subinvalue(char *command, table *t){
    int val, len;
    if(strcmp(commandtype(command), "A_COMMAND") == 0){
        val = getaddress(symbol(command), t) ;
        len = snprintf(NULL, 0, "%d", val);
        char *address= malloc(sizeof(char) * len+1);
        sprintf(address, "%d", val);

        char *cursor = command+1;
        while(*address){
            *cursor = *address;
            cursor++;
            address++;
        }
        *cursor = '\0';
    }
}


int assembler(int argc, char *argv[], char **commands, char *filename, table *t){
    char *out_file = malloc(sizeof(char) *20);

    if (argc != 2)
        printf("%s\n", "Assembler takes one argument.");
    else
        filename = *++argv;

    if((fp = fopen(filename, "r")) == NULL){
        printf("Assembler: can't open %s\n", *argv);
        return 1;
    }else {
        t = make_table(number_of_lines(fp)); 
        commands = getlines(fp);
        build_symbol_table(commands, t);
        //print_contents(t);
        out_file = output_filename(filename);
        fo = fopen(out_file, "w");
        while(*commands != NULL){
            if(strcmp(commandtype(*commands), "L_COMMAND") == 0){
                ++commands;
                continue;
            }
            if(strcmp(commandtype(*commands), "A_COMMAND") == 0){
                if(!isdigit((*commands)[1]))
                    subinvalue(*commands, t);
            }
            fputs(encode(*commands), fo);
            fputs("\n", fo);
            ++commands;
        }
        fclose(fp);
        fclose(fo);
    }
}


int main(int argc, char *argv[])
{
    char **commands;
    char *filename;
    table *t;

    assembler(argc, argv, commands, filename, t);

}


