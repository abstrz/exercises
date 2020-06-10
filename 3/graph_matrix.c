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
delete_vertex(int a, graph_m g){
  int n = dim(g);

  if(a>=0 && a<n){
    if(n==1){
      g[0][0] = 'e';
    }else{

      int i,j;

      free(g[a]);

      for(i=a; i<n-1; i++)
	g[i] = g[i+1];

      g[n-1]=NULL;

      for(i=0;i<n-1;i++){
	for(j=a;j<(n-1);j++)
	  g[i][j] = g[i][j+1]; 
	g[i][n-1] = 'e';
      }
    }
  }
}


graph_m
generate_graph_m(int n){
  graph_m g = (int **) malloc(sizeof(int *));
  g[0] = (int *) malloc(sizeof(int));
  g[0][0] = 'e';

  while((n--)>0)
    insert_vertex(g);

  return g;
}


void
insert_edge(int i, int j, graph_m g){
  int n = dim(g);
  if((i>=0 && i<n) && (j>=0 && j<n))
    g[i][j] = 1;
  else{
    int m = (i>=j) ? i:j;
    printf("%d", m);
    while(n<m+1){
      insert_vertex(g);
      n=dim(g);
    }
    g[i][j]=1;
  }
}

void
delete_edge(int i, int j, graph_m g){
  int n = dim(g);
  if((i>=0 && i<n) && (j>=0 && j<n))
    g[i][j]=0;
}
  

  
  


