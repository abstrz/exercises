#include "interval.h"


/*Exercise 2-1.
 * evaluates to mystery(n) = (n(n^2-1))/3
 * running time is O(n^3)
 */
int mystery(int n)
{
    int r = 0;

    int i,j,k;
    for(i=1;i<n;i++)
        for(j=i+1;j<=n;j++)
            for(k=1; k<=j; k++)
                r++;

    return r;
}

/*Exercise 2-2.
 * evaluates to mystery(n) = (n(n+1)(n+2))/3
 * running time is O(n^3)
 */
int pesky(int n)
{
    int r = 0;

    int i,j,k;
    for(i=1;i<=n;i++)
        for(j=1;j<=i;j++)
            for(k=j; k<=i+j; k++)
                r++;

    return r;
}

/*Exercise 2-3.
 * evaluates to pestiferous(n) = (n(n+1)(n+2)(3n+1))/24
 * running time is O(n^4)
 */
int pestiferous(int n)
{
    int r = 0;

    int i, j, k, l;
    for(i=1;i<=n; i++)
        for(j=1;j<=i;j++)
            for(k=j; k<=(i+j); k++)
                for(l=1; l<=(i+j-k);l++)
                    r++;
    return r;
}

/*Exercise 2-4.
 * If n is even, evaluates to conundrum(n) = n(n+2)(2n-1)/24
 * If n is odd, evaluates to  conundrum(n) = (n-1)(n+1)(2n+3)/24
 * running time is O(n^3)
 */
int conundrum(int n)
{
    int r = 0;

    int i,j,k;
    for(i=1;i<=n;i++)
        for(j=(i+1);j<=n;j++)
            for(k=(i+j-1);k<=n;k++){
                r++;
                printf("i=%d, j=%d, k=%d, r=%d\n", i, j, k, r);
            }

    return r;
}

int
polynomial_eval(int x, int coeffs[])
{
    int i, xpower, p, n;

    n=0;
    i=0;
    while(coeffs[i]){
        n++;
        i++;
    }
    n--;

    p = coeffs[0];
    xpower=1;
    for(i=1;i<=n;i++){
        xpower = x*xpower;
        if(coeffs[i] != 0)
            p += coeffs[i]*xpower;
    }
    return p;
}

int
max(int nums[])
{
    int m = 0;
    for(i=0;i<n;i++)
        if(A[i]> m)
            m = A[i];
    return m;
}




int main()
{
    int *coeffs = malloc(sizeof(int) * 3);
    coeffs[0] = 1;
    coeffs[1] = 5;
    coeffs[2] = 3;
    printf("polynomial_eval(5) = %d",  polynomial_eval(5, coeffs));
}

