#include "graph_matrix.h"

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
dim(graph_m g){
  int n=0;
  while(g[0][n] != -1)
    n++;
  return n;
}

void
printgraph_m(graph_m g){
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

void
insert_vertex(graph_m g){ //add a row and column of zeros.
  int n = dim(g);
  int i;
  size_t size1;
  size_t size2 = sizeof(int)*(n+2);

  if(n>0){
    size1 = sizeof(int*)*(n+1);
    g = realloc(g, size1);

    for(i=0;i<n;i++){
      g[i] = realloc(g[i], size2);
      g[i][n]=0;
      g[i][n+1]=-1;
    }
  }

  //add one final row of zeroes
  g[n] = (int *) malloc(size2);
  g[n][n+1] = -1;
  for(i=0;i<n+1;i++)
    g[n][i] = 0;
}

void
delete_vertex(int a, graph_m g){
  int n = dim(g);

  if(a>=0 && a<n){
    if(n==1){
      g[0][0] = -1;
    }else{

      int i,j;

      free(g[a]);

      for(i=a; i<n-1; i++)
	g[i] = g[i+1];

      g[n-1]=NULL;

      for(i=0;i<n-1;i++){
	for(j=a;j<(n-1);j++)
	  g[i][j] = g[i][j+1]; 
	g[i][n-1] = -1;
      }
    }
  }
}



int has_edge(int i, int j, graph_m g){
  if (dim(g)<=(i>=j ? i : j))
    return 0;
  else if(g[i][j] == 0)
    return 0;
  else
    return 1;
}
    

void
insert_edge(int i, int j, int w, graph_m g){
  int n = dim(g);

  if(w<0)
    w = 0;

  if((i>=0 && i<n) && (j>=0 && j<n)){
    g[i][j] = w;
  }
  else{
    int m = (i>=j) ? i:j;
    while(n<=m){
      insert_vertex(g);
      n=dim(g);
    }
    g[i][j]=w;
  }
}

void
insert_edge_bothways(int i, int j, int w, graph_m g){
  insert_edge(i, j, w, g);
  insert_edge(j, i, w, g);
}

void
delete_edge(int i, int j, graph_m g){
  int n = dim(g);
  if((i>=0 && i<n) && (j>=0 && j<n))
    g[i][j]=0;
}

void
delete_edge_bothways(int i, int j, graph_m g){
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
empty_graph_m(){
  graph_m g = (int **) malloc(sizeof(int *));
  g[0] = (int *) malloc(sizeof(int));
  g[0][0] = -1;

  return g;
}


graph_m
generate_graph_m(int n){
  graph_m g = empty_graph_m();
  
  while((n--)>0)
    insert_vertex(g);
  
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
    
  
graph_m
nearest_neighbor(graph_m g)
{
  int n = dim(g);
  graph_m soln = generate_graph_m(n);
  int *visited = malloc(sizeof(int) * n);

  //  int i, j,row, col, w;
  //
  //
  //  //find minimum neighbor,
  //  //go to it
  //  //repeat
  //  row=0;
  //  col=0;
  //  for(i=0; i<n; i++){
  //    add_int(row+1, visited);
  //    if(len_int_arr(visited) == n){
  //      insert_edge_bothways(row, 0, g[row][0], soln);
  //      break;
  //    }
  //    w = n+2;
  //    //scan row for minimal weight edge, not in graph, preserving cycle.
  //    for(j=0; j<n; j++){
  //      if(g[row][j] > 0 && g[row][j] < w && in_int_arr(j+1, visited) == 0){
  //	col = j;
  //	w = g[row][col];
  //      }
  //    }
  //
  //    if(row<n)
  //      insert_edge_bothways(row, col, w, soln);
  //
  //    row = col;
  //  }
  //
  //  return soln;
}


