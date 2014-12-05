CC = /usr/bin/clang
IFLAGS = -I$$HOME/.local/include
CFLAGS = -Wall -pedantic -std=c99 -ggdb -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wstrict-prototypes -Wmissing-prototypes -Wconversion
LDFLAGS = -lm
TARGET = chip8

all: $(TARGET)

%.o: %.c
	$(CC) $(CFLAGS) $(IFLAGS) -c $<

$(TARGET): $(TARGET).o vm.o file_handling.o
	$(CC) -o $@ $^ $(CFLAGS) $(IFLAGS) $(LDFLAGS)

clean:
	rm $(TARGET) *.o

