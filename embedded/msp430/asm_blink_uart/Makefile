
TARGET := blink
OBJECTS := $(TARGET).o

ARCH	:= msp430
CHIP	:= g2553
CC	    := $(ARCH)-elf-gcc
LD      := $(ARCH)-elf-ld
OBJCOPY := $(ARCH)-elf-objcopy
OBJDUMP := $(ARCH)-elf-objdump
SIZE    := $(ARCH)-elf-size

MCU := -mmcu=$(ARCH)$(CHIP)
CFLAGS = $(MCU) -Os -Wall -I /home/marisa/.local/msp430/include
LDFLAGS = -Wl,-Map,$(TARGET).map -mrelax -Wl,--gc-sections

.PHONY: clean all upload check_splint

all: $(TARGET).elf

%.o: %.S
	$(CC) $(MCU) $(CFLAGS) -c -o $@ $<

$(TARGET).elf: $(OBJECTS)
	$(LD) -o $(TARGET).elf -T msp430.ld $(TARGET).o
	$(OBJDUMP) -d $(TARGET).elf > $(TARGET).lst
	$(SIZE) $(TARGET).elf
	cat $(TARGET).lst

compare: $(TARGET).elf
	$(CC) -o $(TARGET)_c.o $(CFLAGS) $(TARGET).c
	$(SIZE) $(TARGET)_c.o
	$(SIZE) $(TARGET).elf

debug:
	mspdebug rf2500

dump: $(TARGET).elf
	$(OBJDUMP) -d $(TARGET).elf

dumpS: $(TARGET).elf
	$(OBJDUMP) -S $(TARGET).elf

clean:
	rm $(TARGET).lst $(TARGET).elf *.o

upload:
	echo "erase\n load $(TARGET).elf\n exit" | mspdebug rf2500

