#include "graph.h"




int main()
{
    Graph_L g = generate_complete_graph('v', 5);

    chain sol1 = NearestNeighbor(g);
    chain sol2 = ClosestPair(g);
    chain sol3 = OptimalTSP(g);

    printf("First solution has weight: %d\n", sum_weight_chain(sol1));
    printc(sol1);
    printf("Second solution has weight: %d\n", sum_weight_chain(sol2));
    printc(sol2);
    printf("Third solution has weight: %d\n", sum_weight_chain(sol3));
    printc(sol3);
}

