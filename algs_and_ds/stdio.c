#include "stdio.h"
#include "math.h"
#include "ctype.h"

int putchar(int c);


//va_start : start iterating arguments with a va_list
//va_arg: retrieve an argument
//va_end: free a va_list
//va_copy copy contents of one va_list to another

//width:
//The width field specifies a minimum number of characters to output, and is typically used to pad fixed-width fields in tabulated output, where the fields would otherwise be smaller, although it does not cause truncation of oversized fields.
//The width field may be omitted, or a numeric integer value, or a dynamic value when passed as another argument when indicated by an asterisk *,
//for example printf("%*d", 5, 10) will result in 10 being printed, with a total width of 5 characters.

//precision:
//The Precision field usually specifies a maximum limit on the output, depending on the particular formatting type. For floating poiunt numeric types, it specifies the number of digits to the right of the decimal point that the output should be rounded. For the string type, it limits the number of characters that should be output, after which the string is truncated
//The precision field may be omitted, or a numeric integer value, or a dynamic value when passed as another argument when indicated by an asterisk *.
//For example, printf("%.*s", 3, "abcdef") will result in abc being printed.

//length:
//can be omitted or be any of the following:
//hh: For integer types, causes printf to expect an int-sized integer argument wihch was promoted from a char
//h: For int tyes, causes printf to expect an int-sized integer argument which was promoted from a short
//l: For integer types, causes printf to expect a long-sized integer argument. For floating point types, this has no effect.
//ll: For integer types, causes printf to expect a long long-sized integer argument.
//L: For floating point types, causes printf to expect a long double argument.
//z: For integer types, causes printf to expect a size_t-sized integer argument.
//j: For integer types, causes printf to expect a intmax_t-sized integer argument.
//t: For integer types, causes printf to expect a ptrdiff_t-sized integer argument.

//type:
//%: Prints a literal % character (this type doesnt accept any flags, width, precision, length fields.
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
//char is promoted to int, when passed into ...
// "%2424$0.23d"
//For starters will make it just print of the form %specifier.
void prtf(char *s, ...)
{
  va_list args;
  va_list bargs;
  va_list temp;
  int n, parameter, width, precision;
  char flag, length;

  va_start(args, s);
  va_start(bargs, s);

  while (*s != '\0'){
    if( *(s++) == '%' ){

      n=0;
      while(isdigit(*s))
	n += n*10 + (int)(*(s++) - '0'); //523 = 5*10^2 + 2*10 + 3, info missing is the position of this num;
      if(*s == '$'){
	parameter = 1;
	s++;
      }

      if(*s == '-' || *s == '+' *s == ' ' || *s == '0' || *s == '\'' || *(s++) == '#')
	flag = *s;
      else
	flag = 0;
      width = 0;
      while(isdigit(*s))
	width += width*10 + (int)(*(s++) - '0');
      if(*s == '.')
	while(isdigit(*s))
	  precision += precision*10 + (int)(*(s++) - '0');
      if(*s == 'h' && *(s+1) == 'h') //map hh->b and ll->k just to have it fit in a char.
	length = 'b';
      if(*s == 'l' && *(s+1) == 'l')
	length = 'k';
      if(*s == h || *s == l || *s == L ||*s == z || *s == j || *s == t)
	length = *s;


//flags:
//-: left-align the output of this placeholder
//+: prepends a plus for positive signed-numeric types. positive = +, negative = -
// : prepends a space for positive signed-numeric types. positive = , negative = -. This flag is ignored if the + flag exists.
//0: When the 'width' option is specific, prepends zeros for numeric types.
//': The integer or exponent of a decimal has the thousands grouping separator applied.
//#: Alternate form: For g and G types, trailing zeros are not removed. For f,F,eE,g,G types, the output always contains a decimal point.
//   For o,x,X types, the text 0, 0x, 0X, respectively, is prepended to non-zero numbers.
//general format: %[parameter][flags][width][.precision][length]type

      switch(*s){
      case 'c':
	s++;
	char c;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, char);
	  c = va_arg(temp, char);
	}
	else
	  c = va_arg(args, char);
	putchar(c);
	break;
      case 's':
	s++;
	char *;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, char *);
	  str = va_arg(temp, char *);
	}
	else
	  str = va_arg(args, char*);
	while(*str != '\0'){
	  putchar(*str);
	  str++;
	}
	break;
      case 'i':
      case 'd':
	s++;
	int d;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, int);
	  d = va_arg(temp, int);
	}
	else
	  d = va_arg(args, int);
	putchar('0' + d);
	break;
      case 'o':
	s++;
	unsigned int o;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, unsigned int);
	  o = va_arg(temp, unsigned);
	}
	else
	  o = va_arg(args, unsigned int);
	putchar('0' + o);
	break;
      case 'x': 
      case 'X':
	s++;
	unsigned int x;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, unsigned int);
	  x = va_arg(temp, unsigned int);
	}
	else
	  x = va_arg(args, unsigned int);
	putchar('0' + x);
	break;
      case 'u':
	s++;
	unsigned int u;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, unsigned int);
	  u = va_arg(temp, unsigned int);
	}
	else
	  u = va_arg(args, unsigned int);
	putchar('0' + u);
	break;
      case 'f': 
      case 'F':
	s++;
	float f;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, double);
	  f = va_arg(temp, double);
	}
	else
	  f = va_arg(args, double);
	putchar('0' + f);
	break;
      case 'e':
      case 'E':
	s++;
	float e;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, double);
	  e = va_arg(temp, double);
	}
	else
	  e = va_arg(args, double);
	putchar('0' + e);
	break;
      case 'a':
      case 'A':
	s++;	
	float a;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, double);
	  a = va_arg(temp, double);
	}
	else
	  a = va_arg(args, double);
	putchar('0' + a);
	break;
      case 'g':
      case 'G':
	s++;
	float g;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, double);
	  g = va_arg(temp, double);
	}
	else
	  g = va_arg(args, double);
	putchar('0' + g);
	break;
      case 'n':
	s++;
	float na;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, int);
	  na = va_arg(temp, int);
	}
	else
	  na = va_arg(args, int);
	putchar('0' + na);
	break;
      case 'p':
	s++;
	char *p;
	if(parameter){
	  temp = va_copy(temp, bargs);
	  while((n--)>0)
	    va_arg(temp, char *);
	  p = va_arg(temp, char *);
	}
	else
	  p = va_arg(args, char *);
	break;
      default: //if you hit some other %type, then just skip over %type.
	s++ 
	break;
      }
    }
    else{
      putchar(*s);
      s++;
    }
  }

  va_end(args);
}
