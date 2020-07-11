#include "interval.h"

 void
rand_init()
{
    static int init_identifier=0;

    if(init_identifier == 0){
        srand(time(NULL));
        init_identifier++;
    }
}


/*********** interval **********/
interval *
make_interval(int left, int right)
{
    if(left<=right){
        interval *I = malloc(sizeof(interval));
        I->a = left;
        I->b = right;
        return I;
    }else
        return NULL;
}

void
printi(interval *I)
{
    printf("*******************************\n");
    printf("[%d, %d] \n\n", I->a, I->b);
    printf("*******************************\n");
}


int 
width(interval *I)
{
    return I->b - I->a;
}


int 
in_interval(int n, interval *I)
{
   return (n>=(I->a) && n<=(I->b)) ? 1: 0;
}

int 
contained(interval *I, interval *J)
{
    return (in_interval(I->a, J) && in_interval(I->b, J)) ? 1 : 0;
}

interval *
intersection(interval *I, interval *J)
{
    if(contained(I,J))
        return I;
    else if(contained(J,I))
        return J;
    else if(in_interval(I->a, J) && !in_interval(I->b, J)){
        interval *S = malloc(sizeof(interval));
        S->a = I->a;
        S->b = J->b;
        return S;
    }else if(in_interval(J->a, I) && !in_interval(J->b, I)){
        interval *S = malloc(sizeof(interval));
        S->a = J->a;
        S->b = I->b;
        return S;
    }else
        return NULL;
}

/*********** intervals **********/

int
num_intervals(interval **Is)
{
    int n = 0;
    while(*Is){
        n++;
        Is++;
    }
    return n;
}
interval **
generate_intervals(int n)
{
    rand_init();

    int i, left, right;

    interval **Is = malloc(sizeof(interval*) * (n+1));

    for(i=0;i<n;i++){
        left = 1;
        right = 0;
        interval *I = malloc(sizeof(interval));
        while(left>right){
            left = (rand() % 100*n);
            right = (rand() % 100*n);
        }
        I->a = left;
        I->b = right;
        *(Is+i) = I;
    }
    return Is;
}
int
has_interval(interval *I, interval **Is)
{
    while(*Is){
        if(((*Is)->a == I->a) && ((*Is)->b == I->b))
            return 1; 
    }
    return 0;
}
void
delete_interval(interval *I, interval **Is)
{
    if(has_interval(I, Is)){
        while(*Is){
            if(((*Is)->a == I->a) && ((*Is)->b == I->b)){
                if(!*(Is+1)){
                    *Is = NULL;
                    free(*Is);
                }
                else{
                    *Is = *(Is+1);
                    Is++;
                    while(*Is){
                    }
                    Is++;
                }
            }
        }
    }
}

/*********** algorithms **********/

    interval **
EarliestJobFirst(interval **Is)
{
    interval **sol = malloc(sizeof(interval*)*num_intervals(Is));
    interval **ptr;

    int left;

    ptr = Is;

    while(*ptr){


        //find minimal left endpoint corresponding to intervals that do not overlap

    }
}
