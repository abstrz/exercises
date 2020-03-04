/* REPRESENTATION OF GRAPH AS ADJACENCY LIST */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef struct Node Node;
struct Node{
    char *vertex;
    int weight;
    Node *next;
};

typedef Node **Graph_L;

/* A note: There is a difference in how undirected and directed graphs
 * will be handled, but that is up to the user to implement.
 */

void
print_vertices(Graph_L g);

int
has_vertex(char *vertex, Graph_L g);

Node *
lookup(char *vertex, Graph_L g);

void
add_vertex(char *v, Graph_L g);

void
delete_vertex(char *v, Graph_L g);

//checks if edge v1v2 in graph.
int
has_edge(char *v1, char *v2, Graph_L g);

void
add_edge(char *v1, char *v2, int w, Graph_L g);

void
delete_edge(char *v1, char *v2, Graph_L g);
