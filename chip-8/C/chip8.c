#include <stdio.h>
#include <stdint.h>

#define CONV_BYTES_TO_I16(A, B) ((unsigned char) ((unsigned char) (A) << 8 | (unsigned char) (B)))

int main(int argc, char *argv[]) {
    int upper, lower;
    int16_t opcode;
    FILE * file;
    if (argc != 2) {
        printf("Wrong number of arguments -- should be one\n");
        return 1;
    }

    file = fopen(argv[1], "r");
    if (!file) {
        printf("Error opening %s\n", argv[1]);
        return 1;
    }
    while ((upper = fgetc(file), lower = fgetc(file)) != EOF) {
        opcode = CONV_BYTES_TO_I16(upper, lower);
        if (opcode == 0x00e0) {
            printf("Clear screen!\n");
        }
    }

    fclose(file);

    return 0;
}

