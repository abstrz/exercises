#include <stdio.h>
#include <stdlib.h>
#include "string.h"

typedef struct entry entry;

struct entry {
    entry *next; //next entry in chain
    char *key;          
    char *val;
};

typedef struct table table;
struct table{
    int size;
    entry **contents;
};
    
table *make_table(int);
unsigned hash(char *, int n);
entry *lookup(char *, table *t);
entry *insert(char *key, char* val, table *t);
void delete(char *, table *t);


