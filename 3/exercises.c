#include <stdio.h>
#include <stdlib.h>
#include "graph_list.h"
#include "graph_matrix.h"

/* TSP Nearest Neighbor Heuristic:
 * returns linked list which is solution path
 * weights of nodes represent weights of edges.
 */
Node * 
NearestNeighbor_L(Graph_L g)
{
    Node *ng = *g;
}


int main()
{
    Graph_L g = malloc(sizeof(Node*)*100);

    add_vertex("a", g);
    add_vertex("b", g);
    add_vertex("c", g);
    add_vertex("d", g);

    add_edge("b", "c", 5, g);
    add_edge("a", "c", 5, g);
    add_edge("c", "a", 6, g);
    add_edge("c", "d", 4, g);
    add_edge("d", "a", 3, g);

    print_vertices(g);
    
    delete_vertex("a", g);

    print_vertices(g);


}
