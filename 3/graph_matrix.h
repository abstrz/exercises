typedef int **graph_m; 

int dim(graph_m g);
void printgraph_m(graph_m g);
void insert_vertex(graph_m g); //add a row and column of zeros.
void delete_vertex(int i, graph_m g); /* delete ith vertex 
				       ** resultant graph is isomorphic to graph where we 
                                       ** simply delete the ith vertex and remove the edges, 
                                       ** but isn't literally that graph. */
graph_m generate_graph_m(int n);

void insert_edge(int i, int j, graph_m g); //add a row and column of zeros.
void delete_edge(int i, int j, graph_m g); 


