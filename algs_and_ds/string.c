#include "string.h"
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
