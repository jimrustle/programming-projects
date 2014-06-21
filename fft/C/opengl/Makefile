CC=/usr/bin/gcc-4.8
CFLAGS=-Wall -I$$HOME/.local/include -I/usr/include/SDL -Ofast
LDFLAGS=-lm -lfftw3 -lSDL -lGL
TARGET=fft

all: $(TARGET)

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(TARGET).o

.c.o:
	$(CC) $(CFLAGS) -c $<
