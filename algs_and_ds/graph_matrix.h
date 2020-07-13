#include "stdlib.h"
#include "stdio.h"
#include <time.h> //find a better way to implement rand_init
#include "arr.h"

typedef int_arr *graph_m; 

void rand_init();

int dim(graph_m g);
void printgraph_m(graph_m g);

int has_edge(int i, int j, graph_m g);
int connected(int i, int j, graph_m g);
void insert_edge(int i, int j, int w, graph_m g); //add a row and column of zeros.
void insert_edge_bothways(int i, int j, int w, graph_m g); //add a row and column of zeros.
void delete_edge(int i, int j, graph_m g); 
void delete_edge_bothways(int i, int j, graph_m g); //add a row and column of zeros.

int num_adjacent(int i, graph_m g);

int weight(graph_m g);
int_arr *generate_blacklist(graph_m g);
graph_m generate_graph_m(int n);
graph_m generate_complete_graph_m(int n);
int check_cycle(graph_m soln);


graph_m nearest_neighbor(graph_m g);
graph_m closest_pair(graph_m g);

