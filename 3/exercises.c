#include <stdio.h>
#include <stdlib.h>
#include "graph_list.h"
#include "graph_matrix.h"



int main()
{
    Graph_L g = malloc(sizeof(Node*)*100);

    add_node("a", g);
    add_node("b", g);
    add_node("c", g);
    add_node("d", g);

    add_edge("a", "b", g);
    add_edge("b", "c", g);
    add_edge("c", "d", g);
    add_edge("d", "a", g);
    add_edge("c", "a", g);
     
    print_vertices(g);

    remove_edge("c", "a", g);
    print_vertices(g);
    remove_edge("c", "a", g);
    print_vertices(g);
    remove_edge("c", "a", g);
    print_vertices(g);

    printf("Edge from %s to %s: %d \n", "b", "a", has_edge("b","a", g));



}
