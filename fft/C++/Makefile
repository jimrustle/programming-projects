CC=/usr/bin/g++-4.8
#CFLAGS=-Wall -I$$HOME/.local/include -I/usr/include/SDL -pedantic -std=c99 -march=native -mtune=native -mfpmath=sse -flto -funroll-loops -fomit-frame-pointer -Ofast #-Wextra -Werror
CFLAGS=-Wall -I$$HOME/.local/include -pedantic -std=c++11 -g -Ofast
LDFLAGS=-lGL -lglfw -lfftw3
TARGET=mpd

all: $(TARGET)

$(TARGET).o: $(TARGET).cc $(TARGET).h
	$(CC) $(CFLAGS) -c $<

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(TARGET).o

