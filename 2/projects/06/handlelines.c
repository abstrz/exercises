#include "handlelines.h"

const int MAX_FILE_SIZE = 1000000;
char *clean_front_line(char *line){
    while( *line == ' ' )
        ++line;
    return line;
}

//removes trailing white space and comments

char *clean_end_line(char *line){
    int i, last_non_white_space;

    //remove trailing comments
    for (i = 0;  line[i] != '\0' && line[i] != '/' && line [i+1] != '/'; ++i)
        ;

    line[i] = '\0';

    //remove trailing white space
    for(i=0; line[i] != '\0'; ++i)
        if(line[i] != ' ')
            last_non_white_space=i;
    line[last_non_white_space+1] = '\0';


    //remove trailing newline charaacters
    size_t len = strlen(line)-1;
    if (*line && line[len] == '\n')
        line[len-1] = '\0';

    return line;
}

char *next_command(char *line, int maxline, FILE* fp){
    for (line=clean_front_line(line); 
            *line == '/' && *(line+1) == '/'  ||
            strcmp(line, "\n") == 0 ||
            strcmp(line, "\r\n") == 0 ||
            strcmp(line, "\0" ) == 0;
            line=clean_front_line(fgets(line, maxline, fp)))
        ;
    return clean_end_line(line);
}

char **getlines(FILE* fp){
    char **lines = malloc(sizeof(**lines) * MAX_FILE_SIZE);
    int i;
    char *line;
    line = malloc(sizeof(*line) *100);

    for(i=0; fgets(line, 100, fp) != NULL; i++){
        char *newline;
        newline = malloc(sizeof(*newline) *100);
        strcpy(newline, next_command(line, 100, fp));
        lines[i]=newline;
    }
    return lines;
}

int number_of_lines(FILE* fp){
    int number_lines = 0;
    char *line;
    line = malloc(sizeof(*line)*100);

    rewind(fp);

    while (fgets(line, 100, fp) != NULL)
        ++number_lines;

    rewind(fp);

    return number_lines;
}
