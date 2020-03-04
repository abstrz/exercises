#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include "graph_list.h"
#include "graph_matrix.h"

/* TSP Nearest Neighbor Heuristic:
 * returns linked list which is solution cycle
 * weights of nodes represent weights of edges.
 */
Node * 
NearestNeighbor_L(Graph_L g)  //takes as argument a complete, weighted graph on n vertices.
{
    const int min_weight_init = 1000000;
    int min_weight = min_weight_init;

    Node *solution = malloc(sizeof(Node*));

    solution->vertex = (*g)->vertex;

    Node *front_ptr = solution;
   
    Node *nd = (*g);
    char *v0 = nd->vertex;
    while(*(g+1)){
        while (nd->next){
            nd = nd->next;
            if(nd->weight<min_weight && strcmp(nd->vertex, v0) != 0){
                Node *cp = malloc(sizeof(Node*));
                cp->vertex = nd->vertex;
                cp->weight = nd->weight;
                solution->next = cp;
                min_weight = nd->weight;
            }
        }
        if(strcmp(solution->vertex, v0) != 0)
            delete_vertex_and_edges(solution->vertex, g);

        solution = solution->next;
        nd = lookup(solution->vertex, g);
        min_weight = min_weight_init;


    }
    front_ptr->weight = (*g)->next->weight;
    solution->next = front_ptr;

    solution = front_ptr;

    return solution;

}


int main()
{

    Graph_L g = malloc(sizeof(Node*)*100);

    Node *soln;

    add_vertex("a", g);
    add_vertex("b", g);
    add_vertex("c", g);
    add_vertex("d", g);


    add_edge_undirected("a", "b", 2, g);
    add_edge_undirected("a", "c", 4, g);
    add_edge_undirected("a", "d", 1, g);
    add_edge_undirected("b", "c", 5, g);
    add_edge_undirected("b", "d", 3, g);
    add_edge_undirected("c", "d", 99, g);


    soln = NearestNeighbor_L(g);
    print_vertices(g);



    int i=0;
    char * v0 = soln->vertex;

    while(i<3){
        if(strcmp(soln->vertex, v0))
            i++;
        printf("Vertex: %s, Weight:%d\n", soln->vertex, soln->weight); 
        soln= soln->next;
    }



}
