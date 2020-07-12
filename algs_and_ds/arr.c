#include "arr.h"
void
string_arr_print(string_arr arr)
{
  printf("*************************************\n");
  printf("[");
  while(*arr){
    printf("%s,", *arr);
    arr++;
  }
  printf("]\n");
  printf("*************************************\n");
}

void int_arr_print(int_arr arr)
{
  printf("*************************************\n");
  printf("[");
  while(*arr){
    printf("%d, ", *arr);
    arr++;
  }
  printf("]\n", *arr);
  printf("*************************************\n");
}

void int_arrp_print(int_arr *arrp)
{
  while(*arrp){
    int_arr_print(*arrp);
    arrp++;
  }
}


int
string_arr_in(string s, string_arr arr)
{
  while (*arr){
    if (string_cmp(*arr, s) == 0){
      return 1;
    }
    ++arr;
  }
  return 0;
}

int
int_arr_in(int i, int_arr arr)
{
  int k = 0;
  int n = int_arr_len(arr);

  while (k < n){
    if (arr[k] == i)
      return 1;
    k++;
  }
  
  return 0;

}


int
string_arr_len(string_arr s)
{
  int size = 0;
  while(*s){
    size++; s++;
  }

  return size;
}


int
int_arr_len(int_arr arr)
{
  int size = 0;
  while(*arr){
    size++; arr++;
  }

  return size;

}
int
int_arrp_len(int_arr *arr)
{
  int size = 0;
  while(*arr){
    size++; arr++;
  }

  return size;

}

void
string_arr_push(string s, string_arr arr)
{
  while (*arr)
    ++arr;
  *arr = s;
}
//int arrays will use \0 as a terminating character
void
int_arr_push(int i, int_arr arr)
{
  while (*arr != '\0')
    ++arr;
  *arr = i;
}
void
string_arrp_push(string_arr s, string_arr arr[])
{
  while (*arr)
    ++arr;
  *arr = s;
}

int
int_arr_pop(int_arr arr)
{
  int a;
  while(*(arr+1))
    arr++;

  a = *arr;
  *arr = 0;

  return a;
}

int
int_arr_last(int_arr arr)
{
  while(*arr)
    arr++;
  return *(--arr);
}

void
int_arrp_remove_at_index(int i, int_arr *arr)
{
  int n = int_arrp_len(arr);

  if(i>=0 && i<n){
    int m = int_arr_len(arr[i]);
    free(arr[i]);
    arr[i]= (int_arr) malloc(sizeof(int) * m);
  }
}


//returns 1 if unordered arrays are equal, 0 otherwise.
int
string_arr_equal(string_arr s, string_arr t){

  if(string_arr_len(s) == string_arr_len(t)){
    while(*s){
      if(!string_arr_in(*s, t))
	return 0;
      s++;
    }
    return 1;
  }
  else
    return 0;
}


int
int_arr_equal(int_arr arr1, int_arr arr2)
{
  if(int_arr_len(arr1) == int_arr_len(arr2)){
    while(*arr1){
      if(!int_arr_in(*arr1, arr2))
	return 0;
      arr1++;
    }
    return 1;
  }
  else
    return 0;
}


int
string_arrp_in(string_arr s, string_arr *arr)
{
  while (*arr){
    if ((string_cmp(**arr, *s) == 0) && (string_cmp(*(*arr+1), *(s+1)) == 0))
      return 1;
    ++arr;
  }
  return 0;
}


