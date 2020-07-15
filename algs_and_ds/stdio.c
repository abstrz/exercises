#include "stdio.h"
#include "math.h"

int putchar(int c);


//va_start : start iterating arguments with a va_list
//va_arg: retrieve an argument
//va_end: free a va_list
//va_copy copy contents of one va_list to another



//general format: %[parameter][flags][width][.precision][length]type

//parameter:
//n$: 

//type:
//c: character
//s: string
//d or i: converts a signed integer to decimal representation
//o: converts an unsigned integer to octal representation
//X or x: converts an unsigned integer to hexadecimal representation
//u converts an unsigned integer to decimal representation
//F or f: converts floating-point number to the decimal representation
//E or e: converts floating-point number to the decimal exponent notation
//A or a: converts floating-point number to the hexadecimal exponent
//G or g: Converts floating-point number to either decimal or decimal exponent notation
//n: returns the number of characters written so far by this call to the function, the result is written to the value pointed to by the argument.
//p: writes an implementation define character sequence defining a pointer.



//float is promoted to double, when passed into ...
//char is promoted to char, when passed into ...

//For starters will make it just print of the form %specifier.
void prtf(char *s, ...)
{
  va_list args;
  va_start(args, s);
  while (*s != '\0'){
    if( *s == '%' )
      switch(*(s+1)){
      case 'c': ;
	char c = va_arg(args, int);
	putchar(c);
	s = s + 2;
	break;
      case 's': ;
	char *str = va_arg(args, char*);
	while(*str != '\0'){
	  putchar(*str);
	  str++;
	}
	s = s + 2;
	break;
      case 'i':
      case 'd': ;
	int d = va_arg(args, int);
	putchar('0' + d);
	s = s + 2;
	break;
      case 'o': ;
	unsigned int o = va_arg(args, unsigned int);
	putchar('0' + o);
	s = s + 2;
	break;
      case 'x': 
      case 'X': ;
	unsigned int x = va_arg(args, unsigned int);
	putchar('0' + x);
	s = s + 2;
	break;
      case 'u': ;
	unsigned int u = va_arg(args, unsigned int);
	putchar('0' + u);
	s = s + 2;
	break;
      case 'f': 
      case 'F': ;
	float f = va_arg(args, double);
	putchar('0' + f);
	s = s + 2;
	break;
      case 'e':
      case 'E': ;
	float e = va_arg(args, double);
	putchar('0' + e);
	s = s + 2;
	break;
      case 'a':
      case 'A': ;	
	float a = va_arg(args, double);
	putchar('0' + a);
	s = s + 2;
	break;
      case 'g':
      case 'G': ;
	float g = va_arg(args, double);
	putchar('0' + g);
	s = s + 2;
	break;
      case 'n': ;
	float n = va_arg(args, int);
	putchar('0' + n);
	s = s + 2;
	break;
      case 'p': ;
	char *p = va_arg(args, char *);
	s = s + 2;
	break;
      default:
	s++;
	break;
      }
    else{
      putchar(*s);
      s++;
    }
  }

  va_end(args);
}
