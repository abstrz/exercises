#include <stdarg.h>

typedef char *string;

int sprtf(string s, const string format, ...);

string shex(double x);
string sexp(double x);

void stradd(char c, string s);

long unsigned int strlen(string s);
int strmp(string s1, string s2);
string strcpy(string dest, const string src);

//either I write these here, and then important this library to stdio,
//or write the functionality directly into the print function...
//consider if both libraries, string and stdio are loaded together
//you would have in sum more lines of code ... if you write the string_hex, string_exp functions here
//and at the same time implement that functionality as part of printf.
