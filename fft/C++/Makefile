CC = /usr/bin/clang++
#CFLAGS = -Wall -I$$HOME/.local/include -I/usr/include/SDL -pedantic -std=c99 -march=native -mtune=native -mfpmath=sse -flto -funroll-loops -fomit-frame-pointer -Ofast #-Wextra -Werror
CFLAGS = -Wall -I$$HOME/.local/include -pedantic -std=c++11 -O3
LDFLAGS = -lGL -lglfw -lfftw3
TARGET = mpd

all: $(TARGET)

%.o: %.cc %.h
	$(CC) $(CFLAGS) -c $<

$(TARGET): $(TARGET).o drawing.o reading.o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm $(TARGET) *.o

