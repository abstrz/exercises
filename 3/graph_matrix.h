#include <stdlib.h>
#include <stdio.h>
#include <time.h>

typedef int **graph_m; 

void rand_init();

int dim(graph_m g);
void printgraph_m(graph_m g);
void insert_vertex(graph_m g); 
void delete_vertex(int i, graph_m g); 
graph_m generate_graph_m(int n);

int has_edge(int i, int j, graph_m g);
void insert_edge(int i, int j, int w, graph_m g); //add a row and column of zeros.
void insert_edge_bothways(int i, int j, int w, graph_m g); //add a row and column of zeros.
void delete_edge(int i, int j, graph_m g); 
void delete_edge_bothways(int i, int j, graph_m g); //add a row and column of zeros.

graph_m generate_complete_graph_m(int n);
