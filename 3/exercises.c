#include "graph.h"




int main()
{
    Graph_L g = generate_complete_graph('v', 5);
    printc(g);

    chain *cs = acyclic_chains_starting_with((*g)->vertex, g);

    while(*cs){
        printc(*cs);
        cs++;
    }

}

