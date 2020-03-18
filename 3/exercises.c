#include "graph_algorithms.h"

int main()
{
    Graph_L g = generate_complete_graph(100);
    Node *sol = NearestNeighbor_L(g, 100);

    print_solution(sol);
    printf("%d\n", total_distance(sol));
}

