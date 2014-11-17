#include <stdlib.h>
#include <stdint.h>

typedef struct reg {
    int16_t v0, v1, v2, v3, v4, v5, v6, v7,
            v8, v9, vA, vB, vC, vD, vE, vF;
} reg;

typedef struct vm {
    reg registers;
    unsigned char stack[64];
    unsigned char memory[4096];
} vm;

