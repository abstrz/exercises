#include "graph.h"




int main()
{
    Graph_L g = generate_complete_graph('v', 4);
    printc(g);
    chain solution1 = NearestNeighbor(g);
    chain solution2 = ClosestPair(g);

    printc(solution1);




}

