#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "arr.h"

typedef int **graph_m; 

void rand_init();

int dim(graph_m g);
void printgraph_m(graph_m g);
void insert_vertex(graph_m g);
void delete_vertex(int a, graph_m g);
graph_m empty_graph_m();
graph_m generate_graph_m(int n);
graph_m generate_complete_graph_m(int n);

int has_edge(int i, int j, graph_m g);
void insert_edge(int i, int j, int w, graph_m g); //add a row and column of zeros.
void insert_edge_bothways(int i, int j, int w, graph_m g); //add a row and column of zeros.
void delete_edge(int i, int j, graph_m g); 
void delete_edge_bothways(int i, int j, graph_m g); //add a row and column of zeros.

int check_cycle(graph_m soln);


graph_m nearest_neighbor(graph_m g);
