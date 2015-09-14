
#include "ports.h"

int open_port(char* device_file) {
    int fd = open(device_file, O_RDWR | O_NOCTTY | O_NDELAY);
    if (fd != -1) {
        fcntl(fd, F_SETFL, 0);
    }
    return fd;
}

int close_port(int device_file) {
    return close(device_file);
}
