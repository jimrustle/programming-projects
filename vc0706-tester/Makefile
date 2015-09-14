
CC = /usr/bin/clang-3.6
IFLAGS = -I$$HOME/.local/include
CFLAGS = -Wall -pedantic -std=c11 -g -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wstrict-prototypes -Wmissing-prototypes -Wconversion
LDFLAGS = -lm
TARGET = vc0706_test

all: $(TARGET)

%.o: %.c
	$(CC) $(CFLAGS) $(IFLAGS) -c $<

$(TARGET): $(TARGET).o ports.o termios_conf.o
	$(CC) -o $@ $^ $(CFLAGS) $(IFLAGS) $(LDFLAGS)

clean:
	rm $(TARGET) *.o

tidy:
	clang-tidy-3.6 $(TARGET).c -checks='*,clang-analyzer-*' -- -I$$HOME/.local/include   


