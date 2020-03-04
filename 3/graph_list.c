#include "graph_list.h"


void
print_vertices(Graph_L g)
{
    Node *ng;

    while (*g){
        printf("Vertex: %s, Adjacent: ", (*g)->vertex);
        ng = *g;
        while (ng = ng->next){
            printf("%s ", ng->vertex);
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
add_node(char *v, Graph_L g)
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
add_directed_edge(char *v1, char *v2, Graph_L g)
{
    Node *nd1, *nd2;
    if ((nd1 = lookup(v1, g)) != NULL  && has_vertex(v2, g) == 1){
        while (nd1->next){
            nd1 = nd1->next;
        }
        Node *nd2 = malloc(sizeof(Node*));
        nd2->vertex = v2;
        nd2->next = NULL;
        nd1->next = nd2;
    }
}

void
add_edge(char *v1, char *v2, Graph_L g)
{
    add_directed_edge(v1, v2, g);
    add_directed_edge(v2, v1, g);
}


//if we can find v2 amongst the nodes of n1 and v1 amongst the nodes of n2...
void
remove_directed_edge(char *v1, char *v2, Graph_L g)
{
    if(has_edge(v1, v2, g)){
        Node *nd;
        if ((nd = lookup(v1, g)) != NULL && has_vertex(v2, g) == 1)
            while (nd->next->next){
                nd = nd->next;
            }
        nd->next = NULL;
        free(nd->next);
    }
}


void
remove_edge(char *v1, char *v2, Graph_L g)
{
    remove_directed_edge(v1, v2, g);
    remove_directed_edge(v2, v1, g);
}
