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
numbered_vertex(char v, int n){
    char *vertex = malloc(sizeof(char *)* (((int) count_numbers(n)) + 2));


    *vertex = v;

    sprintf(vertex+1, "%d", n);

    return vertex;
}


/*********** CHAIN STUFF **********/

chain
generate_chain(char variable, int n)
{
    rand_init();

    Graph_L nc = malloc(sizeof(Node*) * MAX_GRAPH_SIZE);

    int i,j;

    if(n == 0 || n < 0){
        return NULL;
    }else{
        for(i = 0; i<n; i++)
            add_vertex(numbered_vertex(variable, i), 0, nc);
        for(i = 0; i<n-1; i++)
            add_edge_undirected(numbered_vertex(variable, i), numbered_vertex(variable, i+1), (rand() % (n+1))+1, nc);
    }

    return nc;
}

void
printc(chain c)
{
    Node *nd;

    while (*c){
        printf("Vertex: %s, Adjacent: ", (*c)->vertex);
        nd = *c;
        while (nd->next){
            printf("(%s, %d), ", nd->vertex, nd->weight); 
            nd->next;
        }
        printf("\n");
        ++c;
    }
    printf("\n%s\n", "*****************************");
}

void
add_front_chain(char *v, int w, chain c)
{
    chain p = c;

    while(*(p++) != NULL);

    while(p>=c){
        *(p+1) = *p;
        p--;
    }
    Node *new = malloc(sizeof(Node*));
    new->vertex = v;
    new->weight = 0;

    *c = new;

    add_edge_undirected(v, (*(c+1))->vertex, w, c);
}

void
add_end_chain(char *v, int w, chain c)
{
    add_vertex(v,0,c);
    while(*(c+2))
        c++;
    add_edge_undirected(v, (*c)->vertex, w, c);
}

void
reverse_chain(chain c)
{
    chain p = c;
    Node *temp = malloc(sizeof(Node*));

    while(*(p+1) != NULL)
        p++;

    while(c<p){
        temp = *c;
        *c = *p;
        *p = temp;
        c++;
        p--;
    }
}
    



//pi = 0 means merge i by its starting node.
//   = 1 means merge i by its ending node.
//contents of c2 appended to end of c1.
void
merge_chains(chain c1, chain c2, int w, int p1, int p2)
{
         
    if( p1 == 1 && p2 == 0 ){
        chain start_ptr = c1;
        char *first2 = (*c2)->vertex;
        char *last1;
        while(*(c1+1))
            c1++;
        last1 = (*c1)->vertex;
        c1++;
        while(*c2){
            *c1 = *c2;
            c1++;
            c2++;
        }
        add_edge_undirected(first2, last1, w, start_ptr);
    }else if( p1 == 0 && p2 == 0 ){
        reverse_chain(c1);
        merge_chains(c1, c2, w, 1, 0);
    }else if( p1 == 0 && p2 == 1 ){
        reverse_chain(c1);
        reverse_chain(c2);
        merge_chains(c1, c2, w, 1, 0);
    }else if( p1 == 1 && p2 == 1 ){
        reverse_chain(c2);
        merge_chains(c1, c2, w, 1, 0);
    }else
        printf("%s", "Error! You must enter two chains and two values of either 0 or 1!");
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
add_vertex(char *v, int w, Graph_L g)
{
    if(!has_vertex(v, g)){

        while(*g)
            ++g;

        Node *nd = malloc(sizeof(Node*));
        nd->vertex = v;
        nd->weight = w;
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
generate_complete_graph(char variable, int n)
{
    rand_init();

    Graph_L g = malloc(sizeof(Node*) * MAX_GRAPH_SIZE);

    int i,j;

    for(i = 0; i<n; i++)
        add_vertex(numbered_vertex(variable, i), 0,g);

    for(i = 0; i<n; i++)
        for(j=i+1; j<n; j++){
            add_edge_undirected(numbered_vertex(variable, i), numbered_vertex(variable, j), (rand() % (n+1))+1, g);
        }
    return g;
}




