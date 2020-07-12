#include "graph_matrix.h"

//todo,
//formalize method naming.

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
dim(graph_m g)
{
  int n=0;
  if(g[0])
    while(g[0][n] != -1)
      n++;
  else
    return 0;
  return n;
}

void
printgraph_m(graph_m g)
{
  int i,j=0;
  int n = dim(g);

  //find n, the dimension of the square matrix g
  printf("\n\n");
  if(n==0)
    printf("| |");
  else if(n==1)
    printf("| %d |", g[0][0]);
  else{
    for(i=0; i<n; i++)
      for(j=0; j<n; j++)
	if(j==0)
	  printf("| %d  ", g[i][j]);
	else if(j==n-1)
	  printf("%d |\n", g[i][j]);
	else
	  printf("%d  ", g[i][j]);
  }
  printf("\n");
}

int has_edge(int i, int j, graph_m g)
{
  if (dim(g)<=(i>=j ? i : j))
    return 0;
  else if(g[i][j] == 0)
    return 0;
  else
    return 1;
}

int_arr *
generate_blacklist(graph_m g)
{
  int n = dim(g);
  int i;
  int adj;
  int_arr *blacklist = (int_arr *) malloc(sizeof(int_arr) * n);
  for(i=0;i<n;i++)
    blacklist[i] = (int *) malloc(sizeof(int) * num_adjacent(i, g));

  return blacklist;
}


//depth first search?    
//Note: If you pop x from current_path, then you have to filter out all vertices blacklisted by x.
int connected(int s, int e, graph_m g)

{
  int n = dim(g);

  if(s >= n)
    return 0;

  int_arr *blacklist = generate_blacklist(g);
  int_arr current_path = malloc(sizeof(int) * (n+2));
  int i, j, dummy;

  int_arr_push((i=s)+1, current_path);

  while(1){
    if(has_edge(i, e, g))
      return 1;
    if(int_arr_len(blacklist[s]) == num_adjacent(s, g))
      return 0;

    dummy = 0;

    for(j=0;j<n;j++){
      if(has_edge(i, j, g) && !int_arr_in(j+1, current_path) && !int_arr_in(j+1, blacklist[i])){
  	dummy = 1;
  	i = j;
  	int_arr_push(j+1, current_path);
  	break;
      }
    }
    if(!dummy){
      i = int_arr_pop(current_path) - 1;
      int_arrp_remove_at_index(i,  blacklist);
      int_arr_push(i+1, blacklist[int_arr_last(current_path)-1]); //if eval is left to right, might be able to do assignment on same line
      i = int_arr_last(current_path)-1;
    }
  }
}

void
insert_edge(int i, int j, int w, graph_m g)
{
  int n = dim(g);

  if(w<0)
    w = 0;

  if((i>=0 && i<n) && (j>=0 && j<n))
    g[i][j] = w;
}

void
insert_edge_bothways(int i, int j, int w, graph_m g)
{
  insert_edge(i, j, w, g);
  insert_edge(j, i, w, g);
}

void
delete_edge(int i, int j, graph_m g)
{
  int n = dim(g);
  if((i>=0 && i<n) && (j>=0 && j<n))
    g[i][j]=0;
}

void
delete_edge_bothways(int i, int j, graph_m g)
{
  delete_edge(i, j, g);
  delete_edge(j, i, g);
} 

int
num_adjacent(int i, graph_m g)
{
  int a = 0;
  int n = dim(g);

  int j;
  for(j=0; j<n; j++)
    if(g[i][j] != 0)
      a++;

  return a;
}

graph_m
generate_graph_m(int n)
{
  graph_m g = (graph_m) malloc(sizeof(int *) * n);
  int i,j;
  for(i=0;i<n;i++){
    g[i] = (int_arr) malloc(sizeof(int) * (n+1));
    for(j=0;j<n;j++)
      g[i][j]=0;
  }
  for(i=0;i<n;i++)
    g[i][n] = -1;

  return g;
}

graph_m
generate_complete_graph_m(int n)
{
  rand_init();

  graph_m g = generate_graph_m(n);

  int i,j;

  for(i = 0; i<n; i++)
    for(j=i+1; j<n; j++)
      insert_edge_bothways(i, j, (rand() % (n+1))+1, g);
    
  return g;
}

int
check_cycle(graph_m soln)
{
  int n = dim(soln);

  int i;
  for(i=0; i<n; i++)
    if(num_adjacent(i, soln) != 2)
      return 0;

  return 1;
}

int
weight(graph_m g)
{
  int n = dim(g);
  //sum the edge weights in upper (or lower) diagonal.
  int i,j, w;

  w=0;
  for(i=0;i<n-1;i++)
    for(j=i+1; j<n; j++)
      w += g[i][j];

  return w;
}
  
graph_m
nearest_neighbor(graph_m g)
{
  int n = dim(g);
  graph_m soln = generate_graph_m(n);
  int_arr visited = (int_arr) malloc(sizeof(int) * (n+2));

  int i, j,row, col, w;


  //find minimum neighbor,
  //go to it
  //repeat
  row=0;
  col=0;
  for(i=0; i<n; i++){
    int_arr_push(row+1, visited); //shift it just because 0 is interpreted as null pointer, which causes problems with other int arr methods.
    if(int_arr_len(visited) == n){
      insert_edge_bothways(row, 0, g[row][0], soln);
      break;
    }
						       
    w = n+2; //this is the max weight an edge could have
    for(j=0; j<n; j++){ //scan row for minimal weight edge, not in graph, preserving cycle.
      if(g[row][j] > 0 && g[row][j] < w && int_arr_in(j+1, visited) == 0){
	col = j;
	w = g[row][col];
      }
    }

    if(row<n)
      insert_edge_bothways(row, col, w, soln);

    row = col;
  }

  return soln;
}



  
graph_m
closest_pair(graph_m g)
{
  int n = dim(g);
  graph_m soln = generate_graph_m(n);

  int i,j,k,w, row, col;

  for(i=0;i<n;i++){
    w=n+2;
    if(i==n-1){
      //find the endpoints and connect them
      row = -1;
      for(j=0;j<n;j++)
	if(num_adjacent(j, soln) == 1)
	  if(row == -1)
	    row = j;
	  else
	    col = j;
      w = g[row][col];
      insert_edge_bothways(row, col, w, soln);
    }
    else{
      for(j=0;j<n;j++)
	for(k=0;k<n;k++)
	  //find the minimal edge preseving chains
	  if(soln[j][k] == 0 &&
	     g[j][k] > 0 && 
	     g[j][k] < w && //less than minimal
	     num_adjacent(j, soln) < 2 && //is either an isolated vertex or the endpoint of a chain
	     num_adjacent(k, soln) < 2 && //same
	     !connected(j,k, soln)
	     ){
	    //check if the two are connected by a path. 
	    row = j;
	    col = k;
	    w = g[row][col];
	  }
      insert_edge_bothways(row, col, w, soln);
    }
  }

  return soln;
}
