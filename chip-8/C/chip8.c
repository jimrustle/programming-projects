#include <stdio.h>
#include <stdint.h>
#include "file_handling.h"
#include "vm.h"

int main(int argc, char* argv[]) {
    vm v;
    FILE* program = handle_file(argc, argv);

    load_memory(program, &v);

    return 0;
}

