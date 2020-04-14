#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


/*********** type definitions **********/
typedef struct interval interval;
struct interval{
    int a;
    int b;
};

/*********** interval **********/
interval *make_interval(int left, int right);

void printi(interval *I);

int width(interval *I);

int in_interval(int n, interval *I);

int contained(interval *I, interval *J);

interval *intersection(interval *I1, interval *I2);


/*********** intervals **********/
interval **generate_intervals(int n);
void delete_interval(interval *I, interval **Is);

/*********** algorithms **********/
interval **EarliestJobFirst(interval **I);
