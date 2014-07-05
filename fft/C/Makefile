CC=/usr/bin/gcc-4.8
CFLAGS=-Wall -g -I$$HOME/.local/include -I/usr/include/SDL
LDFLAGS=-lm -lfftw3 -lSDL -lGL
#CFLAGS=-lgmpxx -lgmp
TARGET=fft

all: $(TARGET)

$(TARGET).o: $(TARGET).c $(TARGET).h
	$(CC) $(CFLAGS) $(LDFLAGS) -c $<

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(TARGET).o

