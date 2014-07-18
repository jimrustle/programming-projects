CC=/usr/bin/clang
CFLAGS=-g -Wall -Wextra -Werror
LDFLAGS=-lm
TARGET=linked_list

all: $(TARGET)

$(TARGET).o: $(TARGET).c
	$(CC) $(CFLAGS) -c $<

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm -f $(TARGET) $(TARGET).o

