#include "array.h"


int
in_string_arr(char *s, char **arr)
{
        while (*arr){
                    if (strcmp(*arr, s) == 0)
                                    return 1;
                            ++arr;
                                }
            return 0;
}

void
add_string(char *s, char **arr)
{
        if(in_string_arr(s, arr) == 0){
                    while (*arr)
                                    ++arr;
                            *arr = s;
                                }
}
