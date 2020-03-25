#include "graph.h"




int main()
{
    Graph_L g = generate_complete_graph('v', 5);


    chain solution1 = NearestNeighbor(g);
    // chain solution2 = ClosestPair(g);

    printc(g);
    printc(solution1);
    //printc(solution2);




}

