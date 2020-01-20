#define NULL 0
typedef unsigned int size_t;
extern int printf(const char *format, ...);
extern int scanf(const char *format, ...);
extern int getchar(void);
extern void exit(int status);
extern size_t strlen(const char *s);
extern void *malloc(size_t size);
extern void *calloc(size_t nmeb, size_t size);
extern void *realloc(void *ptr, size_t size);
extern char *strcpy (char* strTo, const char* strFrom);
char *strcat (char* strTo, const char* strFrom);

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

    while ((c = getchar())) {
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

typedef struct Array {
    unsigned int length;
    void *array;
} Array;

extern void *__allocArray(unsigned int length, unsigned int typeSize) {
    Array *result = malloc(sizeof(Array));
    result->array = calloc(length, typeSize);
    result->length = length;
    return result;
}

extern void *__allocClass(unsigned int length) {
    return calloc(length, 4);
}
