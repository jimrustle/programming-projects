
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>

#include "vc0706_test.h"
#include "termios_conf.h"
#include "ports.h"

#define CAM_BUFFER_SIZE 100
#define CAMERADELAY 10

uint8_t read_buffer[CAM_BUFFER_SIZE];
uint8_t frameptr = 0;

void quit(void) {
    puts("Quitting due to errors.");
    exit(1);
}

void send_command(int fd, uint8_t cmd, uint8_t args[], uint8_t argn) {
    uint8_t buf[3] = {0x56, 0, cmd};
    write(fd, buf, 3);

    for (uint8_t i = 0; i < argn; i++) {
      write(fd, &args[i], 1);
    }
}

uint8_t read_response(int fd, int size) {
    int i = 0;
    while (read(fd, &read_buffer[i], 1)) {
        i++;
        if (i >= size - 1) break;
    }

    return i;
}

char* get_version(int fd) {
    uint8_t args[] = {0x00};
    send_command(fd, 0x11, args, 1);

    puts("Sent command");

    int n = read_response(fd, CAM_BUFFER_SIZE);
    if (n < 0) {
      puts("Error in reading response.");
      exit(1);
    }
    printf("%d recv bytes\n", n);
    read_buffer[n] = 0;
    return (char*) read_buffer;
}

void read_image(int fd, int n) {
  /* get image size */

  /* get image */
  uint8_t args[] = {0x0C, 0x0, 0x0A,
                    0, 0, frameptr >> 8, frameptr & 0xFF,
                    0, 0, 0, n,
                    CAMERADELAY >> 8, CAMERADELAY & 0xFF};

  send_command(fd, 0x32, args, sizeof(args));
  read_response(fd, 5);
}

int main(int argc, char *argv[]) {
    TASK task = NONE;
    switch (argc) {
        case 0:
        case 1:
            puts("Error in arguments -- should supply:\n"
"1: just the device file name\n"
"2: supply device the file name plus an additional filename for image output");
            break;
        case 2:
            printf("Testing camera on device file: %s\n", argv[1]);
            task = TEST;
            break;
        case 3:
            printf("Capturing an image on device file: %s\n", argv[1]);
            printf("Saving image to filename: %s\n", argv[2]);
            task = SNAP;
            break;
        default:
            puts("Error: too many arguments");
            break;
    }

    if (task == NONE) {
        quit();
    }


    int opened = open_port(argv[1]);
    if (opened == -1) {
        printf("Error in opening %s.\n", argv[1]);
        quit();
    }
    set_termios(opened);

    /*puts("Sleeping after opening port");*/
    /*sleep(5);*/

    if (task == TEST) {       /* get version name */
        get_version(opened);
        for (int i = 0; i < CAM_BUFFER_SIZE; i++) {
            printf("%c", read_buffer[i]);
        }
      /*puts(get_version(opened));*/
    }
    else {                    /* task = SNAP, get image*/
    }
    /* close_port(opened); */
    return 0;
}

