#include "interval.h"

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

