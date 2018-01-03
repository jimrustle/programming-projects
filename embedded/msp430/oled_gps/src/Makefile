
TARGET := oled_gps
OBJECTS := i2c.o printf.o ssd1306.o ds3231.o gps.o $(TARGET).o

ARCH	:= msp430
CHIP	:= g2553
CC	    := $(ARCH)-gcc
OBJCOPY := $(ARCH)-objcopy
OBJDUMP := $(ARCH)-objdump
SIZE    := $(ARCH)-size
COMPILE_HOUR_D := $(shell date +"%H")
COMPILE_MIN_D := $(shell date +"%M")
COMPILE_SEC_D := $(shell date +"%S")
COMPILE_DAY_OF_WEEK_D := $(shell date +"%u")
COMPILE_DAY_OF_MONTH_D := $(shell date +"%d")
COMPILE_MONTH_OF_YEAR_D := $(shell date +"%m")
COMPILE_YEAR_D := $(shell date +"%y")

MCU := -mmcu=$(ARCH)$(CHIP)
CFLAGS = $(MCU) -Os -Wall -pedantic -std=c99 $(WARNS) $(OPTIMS)
LDFLAGS = -Wl,-Map,$(TARGET).map -mrelax -Wl,--gc-sections

WARNS := -Wall -Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wstrict-prototypes -Wmissing-prototypes # -Wconversion -Wno-main
OPTIMS = -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums -finline-limit=3 -fno-inline-small-functions -ffunction-sections -fdata-sections -ffreestanding -Wl,--relax
WITH_SET_TIME := -DSET_TIME -DCOMPILE_SEC_D=$(COMPILE_SEC_D) \
		-DCOMPILE_MIN_D=$(COMPILE_MIN_D) -DCOMPILE_HOUR_D=$(COMPILE_HOUR_D) \
		-DCOMPILE_DAY_OF_WEEK_D=$(COMPILE_DAY_OF_WEEK_D) \
		-DCOMPILE_DAY_OF_MONTH_D=$(COMPILE_DAY_OF_MONTH_D) \
		-DCOMPILE_MONTH_OF_YEAR_D=$(COMPILE_MONTH_OF_YEAR_D) \
		-DCOMPILE_YEAR_D=$(COMPILE_YEAR_D)

.PHONY: clean all upload check_splint

all: $(TARGET).elf

%.o: %.c
	$(CC) $(MCU) $(CFLAGS) -c -o $@ $<

$(TARGET).elf: $(OBJECTS)
	$(CC) $(MCU) $(LDFLAGS) $(OBJECTS) -o $(TARGET).elf
	#$(OBJCOPY) -O ihex $(TARGET).elf $(TARGET).hex
	$(OBJDUMP) -d $(TARGET).elf > $(TARGET).lst
	$(SIZE) $(TARGET).elf

with_time: CFLAGS += $(WITH_SET_TIME)
with_time: $(TARGET).elf

debug:
	mspdebug rf2500

dump: $(TARGET).elf
	cat $(TARGET).lst

clean:
	rm $(TARGET).lst $(TARGET).elf $(OBJECTS) $(TARGET).map

upload:
	echo "erase\n load $(TARGET).elf\n exit" | mspdebug rf2500
