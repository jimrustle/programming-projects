#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "vm.h"

void run_opcode(int16_t op, vm* v) {
    if (op == 0x00e0) {
        printf("Clear screen!\n");
    }
}

void load_memory(FILE* program, vm *v) {
    int8_t byte;
    int i = 0x200;
    while ((byte = (int8_t) fgetc(program)) != EOF) {
        if (i == 0xEA0) {
            puts("Out of memory error -- program exceeds 4096 byte limit");
            exit(1);
        }
        if (byte == 0x00e0) {
            printf("Clear screen!\n");
    }

        v->memory[i] = byte;
        i++;
    }

}
