#include <stdio.h>
#include <stdlib.h>
#include <string.h>


const int MAX_FILE_SIZE;

int number_of_lines(FILE* fp);
char *clean_front_line(char *line);
char *clean_end_line(char *line);
char *next_command(char *line, int maxline, FILE* fp);
char **getlines(FILE* fp);
char *commandtype(char* line);
char *symbol(char* line);
char *dest(char *line);
char *comp(char *line);
char *jump(char *line);
