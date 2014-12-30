CC = /usr/bin/gcc-4.8
CFLAGS = -Wall -I$$HOME/.local/include -pedantic -std=c99 -g
LDFLAGS = -lm -lGL -lglfw -lfftw3
TARGET = glfw_mpd

all: $(TARGET)

%.o: %.c
	$(CC) $(CFLAGS) -c $<

$(TARGET): $(TARGET).o drawing.o reading.o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm $(TARGET) *.o

