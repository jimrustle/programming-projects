
#include "termios_conf.h"

void set_termios(int port) {
    struct termios settings;
    tcgetattr(port, &settings);

    cfsetispeed(&settings, B38400);
    cfsetospeed(&settings, B38400);

    settings.c_cflag &= ~PARENB;
    settings.c_cflag &= ~CSTOPB;
    settings.c_cflag &= ~CSIZE;
    settings.c_cflag |= CS8;
    settings.c_cc[VTIME] = 1;
    settings.c_cc[VMIN] = 0;

    settings.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);
    settings.c_iflag &= ~(IXON | IXOFF | IXANY);

    settings.c_oflag &= ~OPOST;

    tcsetattr(port, TCSANOW, &settings);
}

