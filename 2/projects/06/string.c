#include "string.h"

char * strcpy(char *s, const char *t)
{
    int i= 0;
    while ((*s = *t) != '\0'){
        s++;
        t++;
    }
    return s;
}

long unsigned int strlen(const char *s)
{
    const char *p = s;
    while (*p != '\0')
        p++;
    return p-s;
}

char *strdup(char *s)
{
    char *p;

    p = (char *) malloc(strlen(s)+1); //+1 for '\0'
    if (p != NULL)
        strcpy(p, s);
    return p;
}

int strcmp(char *s, char *t)
{
    for ( ; *s == *t; s++, t++)
        if (*s == '\0')
            return 0;
    return *s - *t;
}

