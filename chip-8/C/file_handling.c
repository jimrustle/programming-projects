#include <stdio.h>
#include <stdlib.h>
#include "file_handling.h"

FILE* handle_file(int argc, char* argv[]) {
    FILE* f;
    if (argc != 2) {
        printf("Wrong number of arguments -- should be one\n");
        exit(1);
    }

    f = fopen(argv[1], "r");

    if (!f) {
        printf("Error opening %s\n", argv[1]);
        exit(1);
    }

    return f;
}
