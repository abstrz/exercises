#/* REPRESENTATION OF GRAPH AS ADJACENCY LIST */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "arr.h"
/*********** Constants **********/
const int MAX_GRAPH_SIZE;


/*********** type definitions **********/
typedef struct Node Node;
struct Node{
    char *vertex;
    int weight;
    Node *next;
};
typedef Node **graph_l;
typedef graph_l chain;
/* A note: There is a difference in how undirected and directed graphs
 * will be handled, but that is up to the user to implement.
 */



/*********** STRING,ARRAY, MATH PROCEDURES *********/
void rand_init();
int count_numbers(int n);
int factorial(int n);
char *numbered_vertex(char v, int n);
void print_solution(Node *sol);
int total_distance(Node *sol);

void addtostartstring(char c, char *s);

/*********** graph_l ***********/
void printg(graph_l g);
int has_vertex(char *vertex, graph_l g);
Node *lookup(char *vertex, graph_l g);
void add_vertex(char *v, int w, graph_l g);
void delete_vertex_and_edges(char *v, graph_l g);
void delete_vertex(char *v, graph_l g);
int has_edge(char *v1, char *v2, graph_l g);
int get_weight(char *v1, char *v2, graph_l g);
void add_edge(char *v1, char *v2, int w, graph_l g);
void add_edge_undirected(char *v1, char *v2, int w, graph_l g);
void delete_edge(char *v1, char *v2, graph_l g);
int num_vertices(graph_l g);
graph_l generate_complete_graph(char variable, int n);

/*********** CHAIN  **********/
void printc(chain c);
chain generate_chain(char variable, int n);
void add_front_chain(char *vertex, int weight, chain c);
void add_back_chain(char *vertex, int weight, chain c);
void reverse_chain(chain c);
char *get_front_chain(chain c);
char *get_back_chain(chain c);
int sum_weight_chain(chain c);

/*********** CHAIN *  **********/
void merge_chains(chain c1, chain c2, int w, int p1, int p2);
void delete_chain(char *v, chain *cs); 
void visit(chain s, char **visited_vertices, graph_l g);
void merge_and_delete(chain *cs, graph_l g); 
int lighter(chain sol1, chain sol2);
chain *acyclic_chains_starting_with(char *v, graph_l g);
chain *generate_acyclic_chains(graph_l g); //generates array of all acyclic chains of n vertices.
chain minimum_acyclic_chain(chain *cs);


/*********** ALGORITHMS  **********/
chain NearestNeighbor(graph_l g);
chain ClosestPair(graph_l g);
chain OptimalTSP(graph_l g);
void test(int n, int num_trials);



