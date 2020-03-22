#include "graph.h"

#define MAX_GRAPH_SIZE 10000

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

void print_solution(Node *sol)
{
    int i=0;
    while(i<2){
        if(strcmp(sol->vertex, "v0") == 0)
            ++i;
        printf("(%s, %d) \n", sol->vertex, sol->weight);
        sol = sol->next;
    }
}


int total_distance(Node *sol)
{
    int i=0;

    int total = sol->weight;
    sol = sol->next;
    while(strcmp(sol->vertex, "v0") != 0){
        total += sol->weight;
        sol = sol->next;
    }
    return total;
}

void
addtostartstring(char c, char *s)
{
    char *p = s;

    while(*(p++) != '\0');

    while(p>=s){
        *(p+1) = *p;
        p--;
    }
    *s = c;
}

int
in_string_arr(char *s, char **arr)
{
        while (*arr){
                    if (strcmp(*arr, s) == 0)
                                    return 1;
                            ++arr;
                                }
            return 0;
}

void
add_string(char *s, char **arr)
{
    if(in_string_arr(s, arr) == 0){
        while (*arr)
            ++arr;
        *arr = s;
    }
}

int
pair_in_string_arr(char **s, char **arr[])
{
        while (*arr){
            if ((strcmp(**arr, *s) == 0) && (strcmp(*(*arr+1), *(s+1)) == 0))
                return 1;
            ++arr;
        }
        return 0;
}

void
add_pair(char **s, char **arr[])
{
    if(pair_in_string_arr(s, arr) == 0){
        while (*arr)
            ++arr;
        *arr = s;
    }
}



/*********** Graph_L STUFF ***********/

//-1 means that no such edge v1v2 exists.
int
get_weight_of_edge(char *v1, char *v2, Graph_L g)
{
    while(*g){
        if (strcmp((*g)->vertex, v1) == 0){
            Node *nd = (*g)->next;
            while(nd){
                if(strcmp(nd->vertex, v2) == 0){
                    return nd->weight;
                }
                nd= nd->next;
            }
        }
        g++;
    }
    return -1;
}

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

int
get_weight(char *v1, char *v2, Graph_L g)
{
    Node *nd;

    if (nd=lookup(v1, g)){
        while(nd){
            if (strcmp(nd->vertex, v2) == 0)
                return nd->weight;
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

int
num_vertices(Graph_L g){
    int size=0;

    while(*(g++))
        size++;

    return size;
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
/*********** CHAIN **********/

chain
generate_chain(char variable, int n)
{
    rand_init();

    Graph_L nc = malloc(sizeof(Node*) * MAX_GRAPH_SIZE);

    int i,j;

    if(n <= 0){
        return NULL;
    }else{
        for(i = 0; i<n; i++)
            add_vertex(numbered_vertex(variable, i), 0, nc);
        for(i = 0; i<n-1; i++);
            //add_edge_undirected(numbered_vertex(variable, i), numbered_vertex(variable, i+1), (rand() % (n+1))+1, nc);
    }

    return nc;
}

void
printc(chain c)
{
    Node *nd;

    while (*c){
        printf("Vertex: %s, Adjacent: ", (*c)->vertex);
        nd = (*c)->next;
        while (nd){
            printf("(%s, %d), ", nd->vertex, nd->weight); 
            nd = nd->next;
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
add_back_chain(char *v, int w, chain c)
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
    

char *get_front_chain(chain c)
{
    return (*c)->vertex;
}
char *get_back_chain(chain c)
{
    while(*(c+1))
        c++;
    return (*c)->vertex;
}


/*********** CHAINS **********/

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

void
delete_chain(char *v, chain *cs)
{
    chain *p = cs;
    
    chain c;
    while(c = *p){
        if(strcmp((*c)->vertex, v) == 0){
            while(*(p+1)){
                *p = *(p+1);
                p++;
            }
        }
        p++;
    }
}

void
merge_and_delete(chain *cs, Graph_L g)
{
    chain c1, c2;
    int min_weight = num_vertices(g)+2;
    int pos1, pos2;

    chain *p1, *p2;

    p1 = cs;
    while(*p1){
        p2 = p1+1;
        char *front_p1, *back_p1; 
        char *front_p2, *back_p2; 

        front_p1 = get_front_chain(*p1);
        back_p1 = get_back_chain(*p1);


        while(*p2){
            front_p2 = get_front_chain(*p2);
            back_p2 = get_back_chain(*p2);

            if(strcmp(front_p1, back_p1) == 0 && strcmp(front_p2, back_p2) == 0){
                //                printf("======================================\n");
                //                printf("front_p1 = %s, \n", front_p1);
                //                printf("front_p2 = %s \n", front_p2);
                //
                int w;
                w = get_weight(front_p1, front_p2, g);
                if(w<min_weight){
                    c1 = *p1;
                    c2 = *p2;
                    pos1 = 1;
                    pos2 = 0;
                    min_weight = w;
                }
                //                printf("w(front_p1, front_p2) = w(%s, %s)= %d\n", front_p1, front_p2, w);
                //                printf("min_weight = %d\n", min_weight);
                //                printf("pos1 = %d\n", pos1);
                //                printf("pos2 = %d\n", pos2);
                //                printf("(*s)->vertex = %s\n", (*s)->vertex );
                //                printf("(*t)->vertex = %s\n", (*t)->vertex );


            }else if(strcmp(front_p1, back_p1) == 0 && !(strcmp(front_p2, back_p2) == 0)){
            }else if(!(strcmp(front_p1, back_p1) == 0) && strcmp(front_p2, back_p2) == 0){
                printf("======================================\n");
                printf("front_p1 = %s, back_p1 = %s \n\n", front_p1, back_p1);
                printf("front_p2 = %s \n", front_p2);
            }else{
                printf("======================================\n");
                printf("front_p1 = %s, back_p1 = %s \n\n", front_p1, back_p1);
                printf("front_p2 = %s, back_p2 = %s \n\n", front_p2, back_p2);
            }
            p2++;
        }
        p1++;
    }

    merge_chains(c1, c2,  min_weight, pos1, pos2);
    delete_chain((*c2)->vertex, cs);
}


/*********** Algorithms ***********/
Node *NearestNeighbor_L(Graph_L g)  //takes as argument a complete, weighted graph on n vertices.
{
    int n = num_vertices(g);
    const long int min_weight_init = n+2;
    int min_weight = num_vertices(g);


    char **visited = malloc(sizeof(char*) * MAX_GRAPH_SIZE);

    Node *solution = malloc(sizeof(Node*));

    solution->vertex = (*g)->vertex;

    Node *front_ptr = solution;

    Graph_L ptr = g;
    Node *nd = (*g);
    while(*(g++)){
        while (nd->next){
            nd = nd->next;
            if(nd->weight<min_weight && in_string_arr(nd->vertex, visited) == 0){
                Node *cp = malloc(sizeof(Node*));
                cp->vertex = nd->vertex;
                cp->weight = nd->weight;
                solution->next = cp;
                min_weight = nd->weight;
            }
        }

        add_string(solution->vertex, visited);

        if(solution->next == NULL)
            break;

        solution = solution->next;
        nd = lookup(solution->vertex, ptr);
        min_weight = min_weight_init;
    }
    g= ptr;
    front_ptr->weight = (*g)->next->weight;
    solution->next = front_ptr;

    solution = front_ptr;
    return solution;

}
chain *ClosestPair(Graph_L g)
{

    int n = num_vertices(g);

    chain *cs = malloc(sizeof(chain)*MAX_GRAPH_SIZE);

    //build n chains each with one node, (vj, 0)-> pointing to nothing
    int i;
    for(i=0;i<n; i++){
        //create a new node, with *g's vertex and weight values.
        chain c = malloc(sizeof(Node*)*MAX_GRAPH_SIZE);
        add_vertex((*(g+i))->vertex, 0, c);
        *(cs+i) = c;
    }

    merge_and_delete(cs, g);
    merge_and_delete(cs, g);

    return cs;
}


