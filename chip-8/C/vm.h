#include <stdint.h>
#include <stdio.h>

typedef struct reg {
    int16_t v0, v1, v2, v3, v4, v5, v6, v7,
            v8, v9, vA, vB, vC, vD, vE, vF;
} reg;

typedef struct vm {
    reg registers;
    int8_t stack[64];
    int8_t memory[4096];
} vm;

void run_opcode(int16_t op, vm* v);

void load_memory(FILE* program, vm* v);

