CC=/usr/bin/gcc-4.8
CFLAGS=-Wall -I/usr/include/SDL -std=c99 -I$$HOME/.local/include ./kiss_fft130/kiss_fft.c ./kiss_fft130/tools/kiss_fftr.c
LDFLAGS=-lm -lSDL
TARGET=fft_sdl

all: $(TARGET)

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(TARGET).o

.c.o:
	$(CC) $(CFLAGS) $(LDFLAGS) -c $<
