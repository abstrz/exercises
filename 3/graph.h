/* REPRESENTATION OF GRAPH AS ADJACENCY LIST */
#include <stdio.h>
#include <stdlib.h>
#include "string.h"
#include <time.h>



/*********** GENERAL STUFF **********/
extern const int MAX_GRAPH_SIZE;

int
count_numbers(int n);

char *
numbered_vertex(int n);

/*********** NODE STUFF **********/

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

char *
numbered_vertex(int n);

/*********** CHAIN STUFF **********/


void printc(chain c);

chain generate_chain(int n);

chain merge(chain c1, chain c2, chain *c);

//location=0 means add vertex to start of c,
//         1 means add vertex to end of
//
void 
add_front_chain(char *vertex, int weight, chain c);

void 
add_end_chain(char *vertex, int weight, chain c);

/*********** Graph_L STUFF ***********/


void
printg(Graph_L g);

int
has_vertex(char *vertex, Graph_L g);

Node *
lookup(char *vertex, Graph_L g);

void
add_vertex(char *v, int w, Graph_L g);

void
delete_vertex_and_edges(char *v, Graph_L g);

void
delete_vertex(char *v, Graph_L g);

//checks if edge v1v2 in graph.
int
has_edge(char *v1, char *v2, Graph_L g);

void
add_edge(char *v1, char *v2, int w, Graph_L g);

void
add_edge_undirected(char *v1, char *v2, int w, Graph_L g);

void
delete_edge(char *v1, char *v2, Graph_L g);


Graph_L
generate_complete_graph(int n);



