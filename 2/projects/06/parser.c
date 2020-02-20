#include "parser.h"

const int MAX_FILE_SIZE = 1000000000;
const int MAX_LINE = 1000;


int number_of_lines(FILE* fp){
    int number_lines = 0;
    char *line = malloc(sizeof(char) * MAX_LINE);

    rewind(fp);

    while (fgets(line, MAX_LINE, fp) != NULL)
        ++number_lines;

    rewind(fp);

    return number_lines;
}

char *clean_front_line(char *line){
    while( *line == ' ' )
        ++line;
    return line;
}

//removes trailing comments, newline characters, and white space.
char *clean_end_line(char *line){
    int i, end_selector;

    //remove trailing comments
    for (i = 0;  line[i] != '\0' && line[i] != '/' && line [i+1] != '/'; ++i)
        ;
    line[i] = '\0';

    //remove trailing newline characters
    size_t len = strlen(line)-1;
    if (*line && line[len] == '\n')
        line[len-1] = '\0';

    //remove trailing white space
    for(i=0; line[i] != '\0'; ++i)
        if(line[i] != ' ')
            end_selector=i;
    line[end_selector+1] = '\0';

    return line;
}

char *next_line(char *line, FILE* fp){
    for (line=clean_front_line(line); 
            *line == '/' && *(line+1) == '/'  ||
            strcmp(line, "\n") == 0 ||
            strcmp(line, "\r\n") == 0 ||
            strcmp(line, "\0" ) == 0;
            line=clean_front_line(fgets(line, MAX_LINE, fp)))
        ;
    return clean_end_line(line);
}

char **getlines(FILE* fp){
    char **lines = malloc(sizeof(**lines) * MAX_FILE_SIZE);
    int i;
    char *line = malloc(sizeof(char) * MAX_LINE);

    for(i=0; fgets(line, MAX_LINE, fp) != NULL; i++){
        char *newline;
        newline = malloc(sizeof(*newline) * MAX_LINE);
        strcpy(newline, next_line(line, fp));
        lines[i]=newline;
    }
    free(line);
    return lines;
}

char *commandtype(char *line){
    char *command;

    if( line[0] == '@' )
        command = "A_COMMAND";
    else if( line[1] == '=' || line[1] == ';' || line[2] == '=')
        command = "C_COMMAND";
    else if( line[0] == '(' )
        command = "L_COMMAND";
    else 
        NULL;
    
    return command;
}

char *symbol(char *line){
    char *command_type;
    char *s;
    s= malloc(sizeof(*s)*MAX_LINE);
    command_type = commandtype(line);
    if(strcmp(command_type, "A_COMMAND") == 0)
        s= line+1;
    else if(strcmp(command_type, "L_COMMAND") == 0){
        int i;
        for(i=1; line[i] != ')'; i++)
            s[i-1] = line[i];
    }else
        return NULL;
    return s;
}

//dest=comp;jump either of the form comp;jump or dest=comp.

char *dest(char *line){
    char *command_type;
    char *d = malloc(sizeof(char)*3);

    command_type = commandtype(line);
    if(strcmp(command_type, "C_COMMAND") == 0){
        if(line[1] == '=')
            *d = line[0];
        else if (line[2] == '='){
            d[0] = line[0];
            d[1] = line[1];
        }
        else if (line[3] == '='){
            d[0] = line[0];
            d[1] = line[1];
            d[2] = line[2];
        }
        else
            return NULL;
    }else
        return NULL;
    return d;
}       

char *comp(char *line){
    char *command_type;
    char *c;
    
    c = malloc(sizeof(*c));

    command_type = commandtype(line);
    if(strcmp(command_type, "C_COMMAND") == 0){
        if(line[1] == ';'){
            *c = line[0];
        }else if(line[1] == '=')
            c = line+2;
        else if (line[2] == '='){
            c=line+3;
        }
        else if (line[3] == '='){
            c=line+4;
        }
        else
            return NULL;
    }else
        return NULL;
    return c;
}

char *jump(char *line){
    char *command_type;
    char *j;

    j = malloc(sizeof(*j));

    command_type = commandtype(line);
    if(strcmp(command_type, "C_COMMAND") == 0){
        if(line[1] == ';')
            j = line+2;
        else
            return 0;
    }else
        return 0;
    return j;
}
