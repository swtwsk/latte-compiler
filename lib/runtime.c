#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef const char * string;

extern void printInt(long i) {
    printf("%ld\n", i);
}

extern void printString(string str) {
    printf("%s\n", str);
}

extern void error() {
    exit(1);
}

extern long readInt() {
    long i;
    scanf("%ld ", &i);
    return i;
}

// for now
#define BUF_SIZE 1024

string readString() {
    char *line = malloc(sizeof(char) * BUF_SIZE);
    size_t maxLength = BUF_SIZE;
    size_t currLen = 0;
    char c;

    while (c = getchar()) {
        if(c == '\n' || c == '\0')
            break;
        
        line[currLen++] = c;
        if (currLen >= maxLength) {
            line = realloc(line, maxLength * 2 * sizeof(char));
        }
    }

    line = realloc(line, (currLen + 1) * sizeof(char));

    return line;
}

extern string __concatString(string s1, string s2) {
    char *result = malloc(strlen(s1) + strlen(s2) + 1);
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}
