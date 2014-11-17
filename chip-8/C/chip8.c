#include <stdio.h>
#include <stdint.h>

int main(int argc, char *argv[]) {
    char upper, lower;
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

    while ((upper = (char) fgetc(file),
            lower = (char) fgetc(file)) != EOF) {

        opcode = (unsigned char) (upper << 8 | lower);

        if (opcode == 0x00e0) {
            printf("Clear screen!\n");
        }
    }

    fclose(file);

    return 0;
}

