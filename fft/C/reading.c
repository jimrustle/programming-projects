#include "constants.h"
#include <stdio.h>
#include <stdint.h>

void fill_buffer(double *buffer, FILE* fp)
{
    int16_t buf[NUM_DOTS] = {0};
    fread(buf, sizeof(buf[0]), (size_t) NUM_DOTS, fp);

    size_t i;

    for (i = 0; i < NUM_DOTS; i++) {
        buffer[i] = (buf[i] >> 8) + 128.0;
    }
}

