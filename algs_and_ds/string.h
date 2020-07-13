#include <stdarg.h>

typedef char *string;

int sprintf(string s, const string format, ...);

void string_add(char c, string s);

long unsigned int string_len(string s);
int string_cmp(string s1, string s2);
