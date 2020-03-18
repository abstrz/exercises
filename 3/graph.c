#include "graph.h"

const int MAX_GRAPH_SIZE = 10000;



/*********** GENERAL STUFF **********/

void
rand_init()
{
    static int init_identifier=0;

    if(init_identifier == 0){
        srand(time(NULL));
        init_identifier++;
    }
}

int
count_numbers(int n)
{
    int i=0;
    while(n>0){
        n = n/10;
        i++;
    }
    return i;
}

char *
numbered_vertex(int n){
    char *vertex = malloc(sizeof(char *)* (((int) count_numbers(n)) + 2));

    *vertex = 'v';

    sprintf(vertex+1, "%d", n);

    return vertex;
}


/*********** CHAIN STUFF **********/
chain *
add_to_chain(char *vertex, int weight, int location, chain c)
{
    if lookup(vertex, c)
}

/*********** Graph_L STUFF ***********/
void
printg(Graph_L g)
{
    Node *nd;

    while (*g){
        printf("Vertex: %s, Adjacent: ", (*g)->vertex);
        nd = *g;
        while (nd = nd->next){
            printf("(%s, %d), ", nd->vertex, nd->weight);
        }
        printf("\n");
        ++g;
    }
    printf("\n%s\n", "*****************************");
}


int
has_vertex(char *vertex, Graph_L g)
{
    while(*g){
        if(strcmp((*g)->vertex, vertex)==0)
            return 1;
        ++g;
    }
    return 0;
}

Node *
lookup(char *vertex, Graph_L g)
{
    while(*g){
        if(strcmp((*g)->vertex, vertex)==0)
            return *g;
        ++g;
    }
    return NULL;
}

void
add_vertex(char *v, Graph_L g)
{
    if(!has_vertex(v, g)){

        while(*g)
            ++g;

        Node *nd = malloc(sizeof(Node*));
        nd->vertex = v;
        nd->next = NULL;

        *g = nd;
    }
}



void
delete_vertex(char *v, Graph_L g)
{
    Node *nd1, *nd2;
    Graph_L front = g;                  //save initial memory address of g.

    while(*g){
        if ((*g)->vertex == v){
            nd1 = *g;
            *g = NULL;
            while(nd1->next){
                nd2 = nd1;
                nd1= nd1->next;
                free(nd2);
            }
            g = front;
            while(*(g + 1)){        //shift everything to the left by one.
                *g = *(g + 1);
                g++;
            }
            *g = NULL;              //remove final duplicate pointer.
        }
        ++g;
    }
}


void
delete_vertex_and_edges(char *v, Graph_L g)
{
    Node *nd1, *nd2;
    Graph_L front = g;                  //save initial memory address of g.

    delete_vertex(v, g);

    //delete all edges to v
    g= front;
    char *v0;
    while (*g){
        v0 = (*g)->vertex;
        delete_edge(v0, v, g);
        ++g;
    }
}


int
has_edge(char *v1, char *v2, Graph_L g)
{
    Node *nd;

    if (nd=lookup(v1, g)){
        while(nd){
            if (strcmp(nd->vertex, v2) == 0)
                return 1;
            nd = nd->next;
        }
    }
    return 0;
}

void
add_edge(char *v1, char *v2, int w, Graph_L g)
{
    Node *nd1, *nd2;
    if ((nd1 = lookup(v1, g)) != NULL  && has_vertex(v2, g) == 1){
        while (nd1->next){
            nd1 = nd1->next;
        }
        Node *nd2 = malloc(sizeof(Node*));
        nd2->vertex = v2;
        nd2->weight = w;
        nd2->next = NULL;
        nd1->next = nd2;
    }
}

void
add_edge_undirected(char *v1, char *v2, int w, Graph_L g)
{
    add_edge(v1, v2, w, g);
    add_edge(v2, v1, w, g);
}

//if we can find v2 amongst the nodes of n1 and v1 amondst the nodes of n2...
void
delete_edge(char *v1, char *v2, Graph_L g)
{
    if(has_edge(v1, v2, g)){
        Node *nd1, *nd2;

        nd1 = lookup(v1, g);

        while(nd1->next != NULL){
            nd2 = nd1;
            nd1 = nd1->next;
            if(nd1->vertex == v2)
                if (nd1->next){
                    nd2->next = nd1->next;
                    free(nd1);
                }
                else{
                    free(nd2->next);
                    nd2->next = NULL;
                }
        }
    }
}

Graph_L
generate_complete_graph(int n)
{
    rand_init();

    Graph_L g = malloc(sizeof(Node*) * MAX_GRAPH_SIZE);

    int i,j;

    for(i = 0; i<n; i++)
        add_vertex(numbered_vertex(i), g);

    for(i = 0; i<n; i++)
        for(j=i+1; j<n; j++){
            add_edge_undirected(numbered_vertex(i), numbered_vertex(j), (rand() % (n+1))+1, g);
        }
    return g;
}




