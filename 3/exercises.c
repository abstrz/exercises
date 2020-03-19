#include "graph_algorithms.h"

void
addtostartstring(char c, char *s)
{
    char *p = s;

    while(*(p++) != '\0');

    while(p>=s){
        *(p+1) = *p;
        p--;
    }
    *s = c;
}






int main()
{
    chain c = generate_chain(1);
    add_end_chain("a", 10, c);
    printg(c);

}

