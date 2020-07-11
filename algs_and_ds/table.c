#include "table.h"

table *make_table(int n){
    table *t = malloc(sizeof(table*));
    t->size = n;

    return t;
}

unsigned hash(char *s, int n)
{
    unsigned hashval;
    for (hashval=0; *s != '\0'; s++)
        hashval = *s + 31 * hashval;
    return hashval % n;
}

void print_contents(table *t){
    int i=0;
    entry *np;
    while(i<(t->size)){
        np=t->contents[i];
        while(np != NULL){
            printf("(key, val) = (%s, %d)\n", np->key, np->val);
            np = np->next;
        }
        i++;
    }
}

int contains(char *s, table *t){
    int i;
    entry *np;

    for(i=0; i<(t->size); i++){
        np=t->contents[i];
        while(np != NULL){
            if (strcmp(np->key, s) == 0)
                return 1;
            np = np->next;
        }
    }
    return 0;
}

//lookup s in table t.
entry *lookup_table(char *s, table *t)
{
    entry *np;

    for (np=t->contents[hash(s, t->size)]; np != NULL; np = np->next)
        if (strcmp(s, np->key) == 0)
            return np; //found
    return NULL;       //not found
}

int getaddress(char *s, table *t) {
    return lookup_table(s,t)->val;
}

//addentry: put (key, val) in table t.
entry *addentry(char *key, int val, table *t)
{
    entry *np;
    unsigned hashval;

    if ((np = lookup_table(key, t)) == NULL){ //not found
        np = (entry *) malloc(sizeof(*np));
        if (np == NULL || (np->key = strdup(key)) == NULL) 
            return NULL;
        hashval=hash(key, t->size);
        np->next = t->contents[hashval];
        t->contents[hashval] = np;
    }
    np->val = val;
    return np;
}


