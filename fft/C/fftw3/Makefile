CC=/usr/bin/gcc-4.8
CFLAGS=-Wall -I$$HOME/.local/include -I/usr/include/SDL
LDFLAGS=-lm -lfftw3 -lSDL
TARGET=fft_sdl

all: $(TARGET)

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(TARGET).o

.c.o:
	$(CC) $(CFLAGS) $(LDFLAGS) -c $<
