CC = /usr/bin/gcc-4.8
#CFLAGS = -Wall -I$$HOME/.local/include -I/usr/include/SDL -pedantic -std=c99 -march=native -mtune=native -mfpmath=sse -flto -funroll-loops -fomit-frame-pointer -Ofast #-Wextra -Werror
CFLAGS = -Wall -I$$HOME/.local/include -pedantic -std=c99
LDFLAGS = -lm -lGL -lglfw -lfftw3
TARGET = glfw_mpd

all: $(TARGET)

%.o: %.c
	$(CC) $(CFLAGS) -c $<

$(TARGET): $(TARGET).o drawing.o reading.o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm $(TARGET) *.o

