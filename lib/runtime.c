#include <stdio.h>
#include <stdlib.h>

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
    scanf("%ld", &i);
    return i;
}

// for now
string readString() {
    char *line = NULL;
    size_t len = 0;

    getline(&line, &len, stdin);

    return line;
}

