#include "code.h"
#include <math.h>
#include <stdio.h>
#include <string.h>


char *dest_encode(char *s){
    if(strcmp(commandtype(s), "C_COMMAND") == 0){
        char *d;
        d = dest(s);
        if ( d == NULL)
            return "000";
        else if( strcmp(d, "MD") == 0 )
            return "011";
        else if( strcmp(d, "AM") == 0 )
            return "101";
        else if( strcmp(d, "AD") == 0 )
            return "110";
        else if( strcmp(d, "AMD") == 0 )
            return "111";
        else if( *d == 'M' )
            return "001";
        else if( *d == 'D' )
            return "010";
        else if( *d == 'A' )
            return "100";
        else
            return "ERROR: Destination can only be one of A, M, or D.";
    }else
        return "000";
}
char *comp_encode(char *s){
    if(strcmp(commandtype(s), "C_COMMAND") == 0){
        char *c;
        c = comp(s);
        if( *c == '0' )
            return "0101010";
        else if( *c == '1' )
            return "0111111";
        else if( strcmp(c, "-1") == 0 )
            return "0111010";
        else if( strcmp(c, "D") == 0 )
            return "0001100";
        else if( strcmp(c, "A") == 0 )
            return "0110000";
        else if( strcmp(c, "!D") == 0 )
            return "0001101";
        else if( strcmp(c, "!A") == 0 )
            return "0110001";
        else if( strcmp(c, "-D") == 0 )
            return "0001111";
        else if( strcmp(c, "-A") == 0 )
            return "0110011";
        else if( strcmp(c, "D+1") == 0 )
            return "0011111";
        else if( strcmp(c, "A+1") == 0 )
            return "0110111";
        else if( strcmp(c, "D-1") == 0 )
            return "0001110";
        else if( strcmp(c, "A-1") == 0 )
            return "0110010";
        else if( strcmp(c, "D+A") == 0 )
            return "0000010";
        else if( strcmp(c, "D-A") == 0 )
            return "0010011";
        else if( strcmp(c, "A-D") == 0 )
            return "0000111";
        else if( strcmp(c, "D&A") == 0 )
            return "0000000";
        else if( strcmp(c, "D|A") == 0 )
            return "0010101";
        else if( strcmp(c, "M") == 0 )
            return "1110000";
        else if( strcmp(c, "!M") == 0 )
            return "1110001";
        else if( strcmp(c, "-M") == 0 )
            return "1110011";
        else if( strcmp(c, "M+1") == 0 )
            return "1110111";
        else if( strcmp(c, "M-1") == 0 )
            return "1110010";
        else if( strcmp(c, "D+M") == 0 )
            return "1000010";
        else if( strcmp(c, "D-M") == 0 )
            return "1010011";
        else if( strcmp(c, "M-D") == 0 )
            return "1000111";
        else if( strcmp(c, "D&M") == 0 )
            return "1000000";
        else if( strcmp(c, "D|M") == 0 )
            return "1010101";
        else
            return "UNRECOGNIZED COMPUTATION!";
    }else
        return "0000000";
}
char *jump_encode(char *s){
    if(strcmp(commandtype(s), "C_COMMAND") == 0){
        char *j;
        j = jump(s);
        if ( j == NULL)
            return "000";
        else if( strcmp(j, "JGT") == 0 )
            return "001";
        else if( strcmp(j, "JEQ") == 0 )
            return "010";
        else if( strcmp(j, "JGE") == 0 )
            return "011";
        else if( strcmp(j, "JLT") == 0 )
            return "100";
        else if( strcmp(j, "JNE") == 0 )
            return "101";
        else if( strcmp(j, "JLE") == 0 )
            return "110";
        else if( strcmp(j, "JMP") == 0 )
            return "111";
        else
            return "ERROR: JUMP condition not recognized!";
    }else
        return "000";
}

char *encode(char *s){
    char *binary_rep, *c, *d, *j;
    binary_rep = malloc(sizeof(char)*18);

    if (strcmp(commandtype(s), "C_COMMAND") == 0){
        c = comp_encode(s);
        d = dest_encode(s);
        j = jump_encode(s);

        strcat(binary_rep, "111");
        strcat(binary_rep, c);
        strcat(binary_rep, d);
        strcat(binary_rep,j);
    }
    else if (strcmp(commandtype(s), "A_COMMAND") == 0){
        binary_rep = to_binary(symbol(s));
    }
    else
        return 0;
    return binary_rep;
}


char *to_binary(char *s){
    int i, n;
    char *binary = malloc(sizeof(char) * 1000);
    char *start, *end;

    sscanf(s, "%d", &n);

    if(n>32767)
        return 0;
    else{
        for(i=0; n!=0; i++){
            binary[i] = n%2 + '0';
            n = n/2;
        }
        for(i=15; i>-1; i--){
            if(binary[i] != '1')
                binary[i] = '0';
            else 
                break;
        }
        start = end = binary;
        while(*(++end))
            ;
        --end;

        for(; start<end; start++, end--){
            char h = *start;
            char t = *end;
            *start = t;
            *end = h;
        }
    }
    return binary;
}
