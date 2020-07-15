#include <stdlib.h>
#include <stdio.h>
#include "string.h"


typedef int *int_arr; 
typedef string *string_arr; 

void string_arr_print(string_arr arr);
void int_arr_print(int_arr arr);
void int_arrp_print(int_arr *arrp);

int string_arr_len(string_arr s);
int int_arr_len(int_arr arr);
int int_arrp_len(int_arr *arr);

int string_arr_equal(string_arr s, string_arr t);
int int_arr_equal(int_arr arr1, int_arr arr2);

int string_arr_in(string s, string_arr arr);
int int_arr_in(int i, int_arr arr);

int int_arr_last(int_arr arr);



void string_arr_push(string s, string_arr arr);
void int_arr_push(int i, int_arr arr);
void string_arrp_push(string_arr s, string_arr *arr);

int int_arr_pop(int_arr arr);

void int_arr_add_at_index(int i, int_arr arr);

void int_arrp_remove_at_index(int i, int_arr *arr);





