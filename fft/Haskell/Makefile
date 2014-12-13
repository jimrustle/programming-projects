CC = /usr/bin/ghc
TARGET = mpd-display
CFLAGS = -O2

all: $(TARGET)

$(TARGET): $(TARGET).hs
	$(CC) $(CFLAGS) $^

clean:
	rm $(TARGET) *.o

