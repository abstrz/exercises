#include "graph_matrix.h"
#include <stdlib.h>
#include <stdio.h>

int
dim(graph_m g){
  int n=0;
  while(!(g[0][n] == 'e'))
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
  size_t size = (sizeof(int) * (n+2));

  //resize existing rows, and set last entry to zero in each.
  for(i=0;i<n;i++){
    g[i] = (int *) realloc(g[i], size);
    g[i][n]=0;
    g[i][n+1]='e';
  }
  //add one final row of zeroes
  g[n] = (int *) malloc(size);
  g[n][n+1] = 'e';
  for(i=0;i<n+1;i++)
    g[n][i] = 0;
}

void
delete_vertex(int i, graph_m g);

graph_m
generate_graph_m(int n){
  graph_m g = (int **) malloc(sizeof(int *));
  g[0] = (int *) malloc(sizeof(int));
  g[0][0] = 'e';

  while((n--)>=0){
    insert_vertex(g);
  }
  printgraph_m(g);

  return g;
}


void
insert_edge(int i, int j, graph_m g){};

void
delete_edge(int i, int j, graph_m g){};
  

  
  


