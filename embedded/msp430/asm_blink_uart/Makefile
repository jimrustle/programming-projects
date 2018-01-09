
TARGET := blink
OBJECTS := $(TARGET).o

ARCH	:= msp430
CHIP	:= g2553
CC	    := $(ARCH)-gcc
LD      := $(ARCH)-ld
OBJCOPY := $(ARCH)-objcopy
OBJDUMP := $(ARCH)-objdump
SIZE    := $(ARCH)-size

MCU := -mmcu=$(ARCH)$(CHIP)
CFLAGS = $(MCU) -Os -Wall
LDFLAGS = -Wl,-Map,$(TARGET).map -mrelax -Wl,--gc-sections

.PHONY: clean all upload check_splint

all: $(TARGET).elf

%.o: %.S
	$(CC) $(MCU) $(CFLAGS) -c -o $@ $<

$(TARGET).elf: $(OBJECTS)
	$(LD) -o $(TARGET).elf -T msp430.ld $(TARGET).o
	$(OBJDUMP) -D $(TARGET).elf > $(TARGET).lst
	$(SIZE) $(TARGET).elf
	cat $(TARGET).lst

compare: $(TARGET).elf
	$(CC) -o $(TARGET)_c.o $(CFLAGS) $(TARGET).c
	$(SIZE) $(TARGET)_c.o
	$(SIZE) $(TARGET).elf

debug:
	mspdebug rf2500

dump:
	$(OD) -d $(TARGET).obj

dumpS:
	$(OD) -S $(TARGET).obj

clean:
	rm $(TARGET).lst $(TARGET).elf *.o

upload:
	echo "erase\n load $(TARGET).elf\n exit" | mspdebug rf2500

