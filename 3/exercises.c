#include "graph_list_algorithms.h"

int main()
{
    Graph_L g = generate_complete_graph(1000);
    Node *sol = NearestNeighbor_L(g, 1000);

    print_solution(sol);
    printf("%d\n", total_distance(sol));


}

