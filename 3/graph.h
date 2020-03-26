#/* REPRESENTATION OF GRAPH AS ADJACENCY LIST */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/*********** Constants **********/
const int MAX_GRAPH_SIZE;


/*********** type definitions **********/
typedef struct Node Node;
struct Node{
    char *vertex;
    int weight;
    Node *next;
};
typedef Node **Graph_L;
typedef Graph_L chain;
/* A note: There is a difference in how undirected and directed graphs
 * will be handled, but that is up to the user to implement.
 */



/*********** STRING,ARRAY, MATH PROCEDURES *********/
void rand_init();
int count_numbers(int n);
char *numbered_vertex(char v, int n);
void print_solution(Node *sol);
int total_distance(Node *sol);

void addtostartstring(char c, char *s);
int in_string_arr(char *s, char **arr);
int len_string_arr(char **s);
void add_string(char *s, char **arr);
int unordered_arr_equal(char **s, char **t);
int pair_in_string_arr(char **s, char **arr[]);
void add_pair(char **s, char **arr[]);

/*********** Graph_L ***********/
void printg(Graph_L g);
int has_vertex(char *vertex, Graph_L g);
Node *lookup(char *vertex, Graph_L g);
void add_vertex(char *v, int w, Graph_L g);
void delete_vertex_and_edges(char *v, Graph_L g);
void delete_vertex(char *v, Graph_L g);
int has_edge(char *v1, char *v2, Graph_L g);
int get_weight(char *v1, char *v2, Graph_L g);
void add_edge(char *v1, char *v2, int w, Graph_L g);
void add_edge_undirected(char *v1, char *v2, int w, Graph_L g);
void delete_edge(char *v1, char *v2, Graph_L g);
int num_vertices(Graph_L g);
Graph_L generate_complete_graph(char variable, int n);

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
void visit(chain s, char **visited_vertices, Graph_L g);
void merge_and_delete(chain *cs, Graph_L g); 
int lighter(chain sol1, chain sol2);
chain *generate_acyclic_chains(int n, Graph_L g); //generates array of all acyclic chains of n vertices.
chain minimum_acyclic_chain(chain *cs);


/*********** ALGORITHMS  **********/
chain NearestNeighbor(Graph_L g);
chain ClosestPair(Graph_L g);
float nearest_vs_closestpair(int n);



