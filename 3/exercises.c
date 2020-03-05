#include "graph_list.h"
#include "graph_matrix.h"
#include "array.h"

const int MAX_GRAPH_SIZE = 100;

Node * 
NearestNeighbor_L(Graph_L g)  //takes as argument a complete, weighted graph on n vertices.
{
    const int min_weight_init = 1000000;
    int min_weight = min_weight_init;

    char **visited = malloc(sizeof(char*) * MAX_GRAPH_SIZE);

    Node *solution = malloc(sizeof(Node*));

    solution->vertex = (*g)->vertex;

    Node *front_ptr = solution;

    Graph_L ptr = g;
    Node *nd = (*g);
    char *v0 = nd->vertex;
    while(*(g++)){
        while (nd->next){
            nd = nd->next;
            if(nd->weight<min_weight && in_string_arr(nd->vertex, visited) == 0){
                Node *cp = malloc(sizeof(Node*));
                cp->vertex = nd->vertex;
                cp->weight = nd->weight;
                solution->next = cp;
                min_weight = nd->weight;
            }
        }
        
        add_string(solution->vertex, visited);

        if(solution->next == NULL)
            break;

        solution = solution->next;
        nd = lookup(solution->vertex, ptr);
        min_weight = min_weight_init;
    }
    g= ptr;
    front_ptr->weight = (*g)->next->weight;
    solution->next = front_ptr;

    solution = front_ptr;

    return solution;

}

int main()
{

    Graph_L g = malloc(sizeof(Node*)* MAX_GRAPH_SIZE);

    Node *soln;

    add_vertex("a", g);
    add_vertex("b", g);
    add_vertex("c", g);


    add_edge_undirected("a", "b", 2, g);
    add_edge_undirected("b", "c", 3, g);
    add_edge_undirected("c", "a", 1, g);


    soln = NearestNeighbor_L(g);
    print_vertices(g);



    int i=0;
    char * v0 = soln->vertex;

    while(i<2){
        if(strcmp(soln->vertex, v0))
            i++;
        printf("Vertex: %s, Weight:%d\n", soln->vertex, soln->weight); 
        soln= soln->next;
    }
}
