#include "string.h"

//int
//sprintf(string s, const string format, ...)
//{
//  va_list args;
//  //parse string for %. if you find it, check the next character, if it is d,f,e,g,f
//  string ptr = (string) malloc(sizeof(char) * string_len(format));
//  while(ptr
//  if(string_cmp(format, "%d"){}
//  if(string_cmp(format, "%f"){}
//  if(string_cmp(format, "%e"){}
//  if(string_cmp(format, "%g"){}
//  if(string_cmp(format, "%f"){}
//}
//}


void
string_add(char c, string s)
{
  string p = s;

  while(*(p++) != '\0');

  while(p>=s){
    *(p+1) = *p;
    p--;
  }
  *s = c;
}

long unsigned int
string_len(string s)
{
  int len=0;

  while(*(s++) != '\0')
    len++;

  return len;
}

int
string_cmp(string s1, string s2)
{
  if( string_len(s1) <  string_len(s2) )
    while(*s1){
      if(*s1 != *s2)
	return *s1 - *s2;
      if( *(s1+1) == '\0' && *(s2+1) != '\0')
	return -((int) *s2);
      s1++;
      s2++;
    }
  else
    while(*s2){
      if(*s1 != *s2)
	return *s1 - *s2;
      if( *(s1+1) != '\0' && *(s2+1) == '\0')
	return (int) *s1;

      s1++;
      s2++;
    }
  return 0;
}
