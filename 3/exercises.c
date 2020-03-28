#include "graph.h"




int main()
{
    Graph_L g = generate_complete_graph('v', 5);

    chain sol1 = NearestNeighbor(g);
    chain sol2 = ClosestPair(g);
    chain sol3 = OptimalTSP(g);

    test(5);

}

