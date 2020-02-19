#include <stdio.h>
#include "string.h"


const int MAX_FILE_SIZE;


char *fgets(char *line, int maxline, FILE *fp);

char *clean_front_line(char *line);
char *clean_end_line(char *line);
char *next_command(char *line, int maxline, FILE* fp);
char **getlines(FILE* fp);
int number_of_lines(FILE* fp);
