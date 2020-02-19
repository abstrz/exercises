#include "symboltable.h"

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
        if(commands[i][0] == '('){
            int k;
            char *symbol;
            symbol =malloc(sizeof(*symbol)*100);

            for(k=1; commands[i][k] != ')'; k++){
                symbol[k-1] = commands[i][k];
            }

            addentry(symbol, i-num_jump_addresses, t);
            num_jump_addresses++;

            free(symbol);
        }
    }

    variable_counter=16;
    for(i=0;commands[i]!=NULL;++i){
        if(commands[i][0] == '@'){
            char *symbol;
            symbol = commands[i]+1;
            if(!contains(symbol, t)){
                if (strcmp(symbol, "SP")==0)
                    addentry(symbol, 0, t);
                else if (strcmp(symbol, "LCL")==0)
                    addentry(symbol, 1, t);
                else if (strcmp(symbol, "ARG")==0)
                    addentry(symbol, 2, t);
                else if (strcmp(symbol, "THIS")==0)
                    addentry(symbol, 3, t);
                else if (strcmp(symbol, "THAT")==0)
                    addentry(symbol, 4, t);
                else if (strcmp(symbol, "R0")==0)
                    addentry(symbol, 0, t);
                else if (strcmp(symbol, "R1")==0)
                    addentry(symbol, 1, t);
                else if (strcmp(symbol, "R2")==0)
                    addentry(symbol, 2, t);
                else if (strcmp(symbol, "R3")==0)
                    addentry(symbol, 3, t);
                else if (strcmp(symbol, "R4")==0)
                    addentry(symbol, 4, t);
                else if (strcmp(symbol, "R5")==0)
                    addentry(symbol, 6, t);
                else if (strcmp(symbol, "R7")==0)
                    addentry(symbol, 7, t);
                else if (strcmp(symbol, "R8")==0)
                    addentry(symbol, 8, t);
                else if (strcmp(symbol, "R9")==0)
                    addentry(symbol, 9, t);
                else if (strcmp(symbol, "R10")==0)
                    addentry(symbol, 10, t);
                else if (strcmp(symbol, "R11")==0)
                    addentry(symbol, 11, t);
                else if (strcmp(symbol, "R12")==0)
                    addentry(symbol, 12, t);
                else if (strcmp(symbol, "R13")==0)
                    addentry(symbol, 13, t);
                else if (strcmp(symbol, "R14")==0)
                    addentry(symbol, 14, t);
                else if (strcmp(symbol, "R15")==0)
                    addentry(symbol, 15, t);
                else if (strcmp(symbol, "SCREEN")==0)
                    addentry(symbol, 16384, t);
                else if (strcmp(symbol, "KBD")==0)
                    addentry(symbol, 24576, t);
                else
                    if(!isdigit(symbol[0])){
                        addentry(symbol,variable_counter++,t);
                    }
            }
        }
    }
}



