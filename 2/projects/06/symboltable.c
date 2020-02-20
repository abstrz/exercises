#include "symboltable.h"
#include "parser.h"

table *make_table(int n){
    table *t;
    entry **contents;

    t->size = n;
    t->contents = malloc(sizeof(**contents)*(t->size));

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

bool contains(char *s, table *t){
    int i;
    entry *np;

    for(i=0; i<(t->size); i++){
        np=t->contents[i];
        while(np != NULL){
            if (strcmp(np->key, s) == 0)
                return true;
            np = np->next;
        }
    }
    return false;
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

int getaddress(char *s, table *t) {
    return lookup(s,t)->val;
}

//addentry: put (key, val) in table t.
entry *addentry(char *key, int val, table *t)
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
    }
    np->val = val;
    return np;
}

void build_symbol_table(char **commands, table *t){
    int i,num_jump_addresses,variable_counter;

    num_jump_addresses = 0;
    for(i=0;commands[i]!=NULL;++i){
        if(strcmp(commandtype(commands[i]), "L_COMMAND") == 0){
            char *s;
            s= symbol(commands[i]);
            addentry(s, i-num_jump_addresses, t);
            num_jump_addresses++;
            free(s);
        }
    }

    variable_counter=16;
    for(i=0;commands[i]!=NULL;++i){
        if(strcmp(commandtype(commands[i]), "A_COMMAND") == 0){
            char *s;
            s = symbol(commands[i]);
            if(!contains(s, t)){
                if (strcmp(s, "SP")==0)
                    addentry(s, 0, t);
                else if (strcmp(s, "LCL")==0)
                    addentry(s, 1, t);
                else if (strcmp(s, "ARG")==0)
                    addentry(s, 2, t);
                else if (strcmp(s, "THIS")==0)
                    addentry(s, 3, t);
                else if (strcmp(s, "THAT")==0)
                    addentry(s, 4, t);
                else if (strcmp(s, "R0")==0)
                    addentry(s, 0, t);
                else if (strcmp(s, "R1")==0)
                    addentry(s, 1, t);
                else if (strcmp(s, "R2")==0)
                    addentry(s, 2, t);
                else if (strcmp(s, "R3")==0)
                    addentry(s, 3, t);
                else if (strcmp(s, "R4")==0)
                    addentry(s, 4, t);
                else if (strcmp(s, "R5")==0)
                    addentry(s, 6, t);
                else if (strcmp(s, "R7")==0)
                    addentry(s, 7, t);
                else if (strcmp(s, "R8")==0)
                    addentry(s, 8, t);
                else if (strcmp(s, "R9")==0)
                    addentry(s, 9, t);
                else if (strcmp(s, "R10")==0)
                    addentry(s, 10, t);
                else if (strcmp(s, "R11")==0)
                    addentry(s, 11, t);
                else if (strcmp(s, "R12")==0)
                    addentry(s, 12, t);
                else if (strcmp(s, "R13")==0)
                    addentry(s, 13, t);
                else if (strcmp(s, "R14")==0)
                    addentry(s, 14, t);
                else if (strcmp(s, "R15")==0)
                    addentry(s, 15, t);
                else if (strcmp(s, "SCREEN")==0)
                    addentry(s, 16384, t);
                else if (strcmp(s, "KBD")==0)
                    addentry(s, 24576, t);
                else
                    if(!isdigit(s[0])){
                        addentry(s,variable_counter++,t);
                    }
            }
        }
    }
}



