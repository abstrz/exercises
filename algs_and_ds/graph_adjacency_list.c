#include "graph.h"
#include "string.h"
#include "math.h"

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

string
numbered_vertex(char v, int n)
{
  string vertex = malloc(sizeof(char)*(count_numbers(n) + 2);

  *vertex = v;

  sprintf(vertex+1, "%d", n);

  return vertex;
}

void 
print_solution(Node *sol)
{
  int i=0;
  while(i<2){
    if(string_cmp(sol->vertex, "v0") == 0)
      ++i;
    printf("(%s, %d) \n", sol->vertex, sol->weight);
    sol = sol->next;
  }
}


int 
total_distance(Node *sol)
{
  int i=0;

  int total = sol->weight;
  sol = sol->next;
  while(string_cmp(sol->vertex, "v0") != 0){
    total += sol->weight;
    sol = sol->next;
  }
  return total;
}





/*********** graph_l STUFF ***********/

void
printg(graph_l g)
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
has_vertex(string vertex, graph_l g)
{
  while(*g){
    if(string_cmp((*g)->vertex, vertex)==0)
      return 1;
    ++g;
  }
  return 0;
}

Node *
lookup(string vertex, graph_l g)
{
  while(*g){
    if(string_cmp((*g)->vertex, vertex)==0)
      return *g;
    ++g;
  }
  return NULL;
}

void
add_vertex(string v, int w, graph_l g)
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
string 
next_vertex(string vertex, graph_l g)
{
  graph_l ptr = g;
  while(*ptr){
    if(string_cmp((*ptr)->vertex, vertex)==0){
      if(*(ptr+1))
	return (*(ptr+1))->vertex;
      else
	return (*g)->vertex;
    }
    ptr++;
  }
  return NULL;
}

int
has_edge(string v1, string v2, graph_l g)
{
  Node *nd;

  if (nd=lookup(v1, g)){
    while(nd){
      if (string_cmp(nd->vertex, v2) == 0)
	return 1;
      nd = nd->next;
    }
  }
  return 0;
}

int
get_weight(string v1, string v2, graph_l g)
{
  Node *nd;

  if (nd=lookup(v1, g)){
    while(nd){
      if (string_cmp(nd->vertex, v2) == 0)
	return nd->weight;
      nd = nd->next;
    }
  }
  return 0;
}


void
add_edge(string v1, string v2, int w, graph_l g)
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
add_edge_undirected(string v1, string v2, int w, graph_l g)
{
  add_edge(v1, v2, w, g);
  add_edge(v2, v1, w, g);
}

//if we can find v2 amongst the nodes of n1 and v1 amondst the nodes of n2...
void
delete_edge(string v1, string v2, graph_l g)
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
num_vertices(graph_l g){
  int size=0;

  while(*(g++))
    size++;

  return size;

}
graph_l
generate_complete_graph(char variable, int n)
{
  rand_init();

  graph_l g = malloc(sizeof(Node*) * MAX_GRAPH_SIZE);

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

  graph_l nc = malloc(sizeof(Node*) * MAX_GRAPH_SIZE);

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
    printf("Vertex: %s Adjacent:", (*c)->vertex);
    nd = (*c)->next;
    while (nd){
      printf(" (%s, %d) ", nd->vertex, nd->weight); 
      nd = nd->next;
    }
    printf("\n");
    ++c;
  }
  printf("\n%s\n", "*****************************");
}
void
add_front_chain(string v, int w, chain c)
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
add_back_chain(string v, int w, chain c)
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

char 
*get_front_chain(chain c)
{
  return (*c)->vertex;
}
char 
*get_back_chain(chain c)
{
  while(*(c+1))
    c++;
  return (*c)->vertex;
}

int
sum_weight_chain(chain c){
  int sum = 0;

  chain ptr = c;
  while(*(ptr+1)){
    sum += get_weight((*ptr)->vertex, (*(ptr+1))->vertex, c);
    ptr++;
  }
  sum += get_weight(get_front_chain(c), get_back_chain(c), c);

  return sum;

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
    string first2 = (*c2)->vertex;
    string last1;
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
delete_chain(string v, chain *cs)
{
  chain *p = cs;

  chain c;
  while(c = *p){
    if(string_cmp((*c)->vertex, v) == 0){
      while(*(p+1)){
	*p = *(p+1);
	p++;
      }
    }
    p++;
  }
}

void
visit(chain c, string *visited_vertices, graph_l g){

  int min_weight = num_vertices(g)+2;
  int w;

  if(*visited_vertices){
    string s, *t;
    if (len_string_arr(visited_vertices) < num_vertices(g)){
      s = get_back_chain(c);
      while(*g){
	if(!in_string_arr((*g)->vertex, visited_vertices)){
	  w= get_weight((*g)->vertex, s, g);
	  if(w<min_weight){
	    min_weight = w;
	    t = (*g)->vertex;
	  }
	}
	g++;
      }
      add_vertex(t, 0, c);
      add_edge_undirected(s, t, min_weight, c);
      add_string(t, visited_vertices);
    }
  }else
    if(*g){
      add_vertex((*g)->vertex, 0, c);
      add_string((*g)->vertex, visited_vertices);
    }
}
void
merge_and_delete(chain *cs, graph_l g)
{
  chain c1, c2;
  int min_weight = num_vertices(g)+2;
  int pos1, pos2;

  chain *p1, *p2;

  p1 = cs;
  while(*p1){
    p2 = p1+1;
    string front_p1, *back_p1; 
    string front_p2, *back_p2; 

    front_p1 = get_front_chain(*p1);
    back_p1 = get_back_chain(*p1);


    while(*p2){
      front_p2 = get_front_chain(*p2);
      back_p2 = get_back_chain(*p2);

      if(string_cmp(front_p1, back_p1) == 0 && string_cmp(front_p2, back_p2) == 0){
	int w;
	w = get_weight(back_p1, front_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 1;
	  pos2 = 0;
	  min_weight = w;
	}
      }else if(string_cmp(front_p1, back_p1) == 0 && !(string_cmp(front_p2, back_p2) == 0)){
	int w;
	w = get_weight(back_p1, front_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 0;
	  pos2 = 0;
	  min_weight = w;
	}
	w = get_weight(back_p1, back_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 1;
	  pos2 = 1;
	  min_weight = w;
	}
      }else if(!(string_cmp(front_p1, back_p1) == 0) && string_cmp(front_p2, back_p2) == 0){
	int w;
	w = get_weight(front_p1, front_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 0;
	  pos2 = 0;
	  min_weight = w;
	}
	w = get_weight(back_p1, front_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 1;
	  pos2 = 1;
	  min_weight = w;
	}

      }else if(!(string_cmp(front_p1, back_p1) == 0) && !(string_cmp(front_p2, back_p2) == 0)){
	int w;
	w = get_weight(back_p1, front_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 1;
	  pos2 = 0;
	  min_weight = w;
	}
	w = get_weight(back_p1, back_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 1;
	  pos2 = 1;
	  min_weight = w;
	}
	w = get_weight(front_p1, back_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 0;
	  pos2 = 1;
	  min_weight = w;
	} 
	w = get_weight(front_p1, front_p2, g);
	if(w<min_weight){
	  c1 = *p1;
	  c2 = *p2;
	  pos1 = 0;
	  pos2 = 0;
	  min_weight = w;
	}
      }else{
	printf("front_p1 = %s, back_p1 = %s \n\n", front_p1, back_p1);
	printf("front_p2 = %s, back_p2 = %s \n\n", front_p2, back_p2);
      }
      p2++;
    }
    p1++;
  }

  merge_chains(c1, c2,  min_weight, pos1, pos2);
  delete_chain((*c2)->vertex, cs);
  while(*(cs+1))
    cs++;
  *cs = NULL;
}
int
lighter(chain sol1,chain sol2)
{
  return sum_weight_chain(sol1)<sum_weight_chain(sol2) ? 1 : 0;
}

chain *
acyclic_chains_starting_with(string v, graph_l g)
{
  if(has_vertex(v, g)){
    int i,j,k, n1, n2, w;
    string new, *front, *back;

    int n = num_vertices(g);
    int num_chains = factorial(n-1);

    chain *sol = malloc(sizeof(chain)*(num_chains+1));
    chain *ptr;


    for(i=0; i<n; i++){
      n2 = factorial(n-1-i);
      n1 = factorial(n-1)/n2;
      for(j=0; j<n1; j++){
	if(!*(sol+j))
	  new=v;
	else
	  while(has_vertex(new, *(sol+n2*j)))
	    new = next_vertex(new, g);
	for(k=n2*j; k<(n2*(j+1)); k++){
	  if(!*(sol+k)){
	    chain c = malloc(sizeof(Node*)*(n+1));
	    add_vertex(new, 0, c);
	    *(sol+k)= c;
	  }else{
	    back = get_back_chain(*(sol+k));
	    add_vertex(new, 0, *(sol+k));
	    add_edge_undirected(back, new, get_weight(back, new, g), *(sol+k));
	  }
	}
	new = next_vertex(new, g);
      }
    }
    ptr = sol;
    while(*ptr){
      front = get_front_chain(*ptr);
      back = get_back_chain(*ptr);
      add_edge_undirected(front, back, get_weight(front,back, g), *ptr);
      ptr++;
    }


    return sol;
  }else
    return NULL;
}

//generates array of all acyclic chains of n vertices.
chain *
generate_acyclic_chains(graph_l g)
{
  int n = num_vertices(g);

  chain *sol = malloc(sizeof(chain)*(factorial(n)+(n+1)));

  chain *temp;

  int i,j, m;

  m = factorial(n-1);
  for(i=0;i<n;i++){
    temp = acyclic_chains_starting_with((*(g+i))->vertex, g);
    for(j=m*i;j<m*(i+1); j++){
      *(sol+j) = *(temp+j - m*i);
    }
  }
  return sol;
}

chain
minimum_acyclic_chain(chain *cs){
  int n = num_vertices(*cs);
  chain solution = malloc(sizeof(Node*) * (n+1));
  int min_weight =  n * (n+2); 
  int w;
  while(*cs){
    w=sum_weight_chain(*cs);
    if(w<min_weight){
      min_weight = w;
      solution = *cs;
    }
    cs++;
  }
  return solution;
}

/*********** Algorithms ***********/
chain
NearestNeighbor(graph_l g)
{
  int n = num_vertices(g);
  string *V_g = malloc(sizeof(char*) * n+1);

  chain solution = malloc(sizeof(Node*) * n+1);
  string *visited_points = malloc(sizeof(char*)*n);

  int i=0;
  while(*(g+i)){
    add_string((*(g+i))->vertex, V_g);
    i++;
  }

  for(i=0;i<n;i++)
    visit(solution, visited_points, g);

  string front = get_front_chain(solution);
  string back = get_back_chain(solution);

  add_edge_undirected(front, back, get_weight(front, back, g), solution);


  return solution;
}


chain ClosestPair(graph_l g)
{

  int n = num_vertices(g);

  chain *cs = malloc(sizeof(chain)*MAX_GRAPH_SIZE);

  string front, *back;

  //build n chains each with one node, (vj, 0)-> pointing to nothing
  int i;
  for(i=0;i<n; i++){
    //create a new node, with *g's vertex and weight values.
    chain c = malloc(sizeof(Node*)*MAX_GRAPH_SIZE);
    add_vertex((*(g+i))->vertex, 0, c);
    *(cs+i) = c;
  }
  while ( *(cs+1) ){
    merge_and_delete(cs, g);
  }

  front = get_front_chain (*cs);
  back = get_back_chain(*cs);

  add_edge_undirected(front, back, get_weight(front, back, g), *cs);


  return *cs;
}

//returns 1 if sol1 is lighter than sol2, and 0 otherwise.


chain 
OptimalTSP(graph_l g)
{
  return minimum_acyclic_chain(generate_acyclic_chains(g));
}


void
test(int n, int num_trials)
{

  graph_l g;

  chain optimal;

  chain sol1, sol2;

  int i, min1, min2, min_weight;
  i = min1= min2 = 0;

  for(;i<num_trials;i++){
    g = generate_complete_graph('v', n);

    optimal = OptimalTSP(g);

    sol1 = NearestNeighbor(g);
    sol2 = ClosestPair(g);

    min_weight = sum_weight_chain(optimal);

    if(sum_weight_chain(sol1) == min_weight)
      min1 ++;
    if(sum_weight_chain(sol2) == min_weight)
      min2 ++;

  }

  printf("The nearest neighbor heuristic on %d vertices produced a minimal weight, maximal vertex cyclic subgraph %f percent of the time.\n", n, 100*(((float) min1)/num_trials));

  printf("The closest pair heuristic on %d vertices produced a minimal weight, maximal vertex cyclic subgraph %f percent of the time.\n", n, 100*(((float) min2)/num_trials));
}






