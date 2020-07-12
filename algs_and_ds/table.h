#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <string.h>

typedef struct entry entry;



struct entry {
    entry *next;
    char *key;          
    int val;
};

typedef struct table table;
struct table{
    int size;
    entry **contents;
};


table *
make_table(int n);

unsigned 
hash(char *key, int n);

void 
print_contents(table* t);

entry *
lookup_table(char *key, table *t);

entry *
addentry(char *key, int val, table *t);

int 
contains(char *key, table *t);

int 
getaddress(char *key, table *t);

