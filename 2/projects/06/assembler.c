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

    for(i=0; name[i] != '.'; i++)
        if( name[i] == '/' )
            cursor = i;
    name[cursor] = 'h';
    name[cursor+1] = 'a';
    name[cursor+2] = 'c';
    name[cursor+3] = 'k';
    name[cursor+4] = '\0';

    return name;

}


int assembler(int argc, char *argv[], char **commands, char *filename, table *t){
    char *out_file;

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
        out_file = output_filename(filename);
        fo = fopen(out_file, "w");
        while(*commands != NULL){
            encode(*commands);
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


