#include "graph.h"




int main()
{
    Graph_L g = generate_complete_graph('v', 10);
    chain *solution = ClosestPair(g);


    while(*solution){
        printc(*solution);
        solution++;
    }

     chain c = generate_chain('v', 5);
     chain d = generate_chain('w', 6);

     merge_chains(c, d, 50, 1, 1);

}

