

typedef struct Node Node;
struct Node{
    char *vertex;
    Node *next;
};

typedef Node **Graph;

/* A note: There is a difference in how undirected and directed graphs
 * will be handled, but that is up to the user to implement.
 */

//inserts node into data structure, and returns the added node, if successful.
Node add_node(Node *nd, Graph g);

//deletes node from graph
void delete_node(Node *nd, Graph g);

//insert edge between nd1 and nd2
void add_edge(Node *nd1, Node *nd2, Graph g);

//delete edge between nd1 and nd2
void delete_edge(Node *nd1, Node *nd2, Graph g);
