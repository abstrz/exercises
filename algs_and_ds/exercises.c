#include "graph_matrix.h"

int main()
{
  graph_m g = generate_complete_graph_m(4);
  printgraph_m(g);
  int_arr *blacklist = generate_blacklist(g);
  int_arrp_print(blacklist);
  
}

