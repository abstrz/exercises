#include "graph_algorithms.h"



int main()
{
    chain c = generate_chain('v', 5);
    chain d = generate_chain('w', 6);
    printg(c);
    printg(d);
    merge_chains(c, d, 50, 0, 1);
    printg(c);

}

