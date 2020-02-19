#include "table.h"

table *make_table(int n){
    table *t;
    entry **contents;

    t->size = n;
    t->contents = malloc(sizeof(**contents)*(t->size));

    return t;
}



//hash: form hash val for string s
unsigned hash(char *s, int n)
{
    unsigned hashval;
    for (hashval=0; *s != '\0'; s++)
        hashval = *s + 31 * hashval;
    return hashval % n;
}

//lookup s in table t.
entry *lookup(char *s, table *t)
{
    entry *np;

    for (np=t->contents[hash(s, t->size)]; np != NULL; np = np->next)
        if (strcmp(s, np->key) == 0)
            return np; //found
    return NULL;       //not found
}

//insert: put (key, val) in table t.
entry *insert(char *key, char *val, table *t)
{
    entry *np;
    unsigned hashval;

    if ((np = lookup(key, t)) == NULL){ //not found
        np = (entry *) malloc(sizeof(*np));
        if (np == NULL || (np->key = strdup(key)) == NULL)
            return NULL;
        hashval=hash(key, t->size);
        np->next = t->contents[hashval];
        t->contents[hashval] = np;
    }else                            //already there
        free(np->val);      //free previous defn
    if ((np->val = strdup(val)) == NULL)
        return NULL;
    return np;
}

void delete(char *key, table *t)
{
    entry *np1, *np2;
    unsigned hashval = hash(key, t->size);

    for (np2 = t->contents[hashval], np1 = NULL;
            np2 != NULL;
            np1 = np2, np2 = np2->next){
        if (strcmp(np2->key, key) == 0){
            if (np1 == NULL)
                t->contents[hashval] = np2->next;
            else
                np1->next = np2->next;
            free(np2->key);
            free(np2->val);
            free(np2);
        }
    }
}






