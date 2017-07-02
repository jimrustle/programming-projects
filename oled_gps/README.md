
# MSP430 GPS SSD1306 OLED Display Thingy

It's a speedometer and a clock.

## MSP430

MSP430G2553 microcontroller - programmed on a Launchpad, then removed and soldered onto a perfboard.

## GPS - u-blox NEO-6M

[gps schematic](doc/gps\_schematic.png)

This GPS module is a u-blox NEO-6M unit that you can buy off eBay for about $10. It also comes with an I2C EEPROM (M24C32, 24AA32A) for storing configuration.

Note that the NEO-6M is discontinued by u-blox. Chinese sellers on eBay still have them available, and also have the newer NEO-7M and NEO-M8 available.

Since we won't be changing the configuration of the GPS, we'll leave the input RX pin on the GPS module disconnected. (Features you can change include things like baud rate, NMEA message outputs, and output update rate)

The GPS module is used to determine vehicular speed, by converting the 'knots' attribute of the GPRMC NMEA message into kilometres per hour. (Note: later editions to support `km/h` attribute of VTG NMEA message)

See also:
 - [NEO-6 data sheet](https://www.u-blox.com/sites/default/files/products/documents/NEO-6\_DataSheet\_%28GPS.G6-HW-09005%29.pdf)
 - [NEO-6 receiver description](https://www.u-blox.com/sites/default/files/products/documents/u-blox6\_ReceiverDescrProtSpec\_%28GPS.G6-SW-10018%29\_Public.pdf)
 - [NEO-6 hardware integration manual](https://www.u-blox.com/sites/default/files/products/documents/LEA-NEO-MAX-6\_HIM\_%28UBX-14054794%29.pdf)

| Property | Notes |
| -------- | ----- |
| Voltage | 3.3 V to 5 V, using 3.3 V |
| Inputs | None |
| Outputs | 9600 baud UART |

## DS3231 RTC

The DS3231 is a real-time clock, allowing the device to keep track of time when powered off and when GPS is disabled. The DS3231 module (~$5, eBay) also comes with a programmable EEPROM, in case you want to store data.

See also:
 - [Usage guide](https://edwardmallon.wordpress.com/2014/05/21/using-a-cheap-3-ds3231-rtc-at24c32-eeprom-from-ebay/)

| Property | Notes |
| -------- | ----- |
| Voltage | 3.3 V to 5 V, using 3.3 V |
| Inputs | I2C |
| Outputs | I2C |
| Address | 0x68 |

## SSD1306 OLED Dispay

This is a 128x64 OLED display, interfaced through I2C. It uses an SSD1306 display driver.

Since our microcontroller has only 512 bytes of RAM, we must use page addressing mode to write to the display. If we had more RAM, it would be feasible to store the entire display buffer on the microcontroller, then writing it out. However, this method takes more time, as each pixel must be written consecutively - page address mode lets you easily define a "block" to draw within, while horizontal/vertical line addressing requires you to write an entire line of the buffer at once.

See also:
 - [SSD1306 display driver datasheet](https://www.olimex.com/Products/Modules/LCD/MOD-OLED-128x64/resources/SSD1306.pdf)

| Property | Notes |
| -------- | ----- |
| Voltage | 3.3 V to 5 V, using 3.3 V |
| Inputs | I2C |
| Outputs | I2C |
| Address | 0x3C |

# Programming/Build Notes

```
.
├── README.md               | this file
├── doc                     |
│   └── gps_diagram.jpg     | GPS module schematic
├── src                     |
│   ├── ds3231.c            | i2c commands for interfacing with the ds3231 real-time clock
│   ├── ds3231.h            |
│   ├── font.h              | font definition, has slight modifications for style
│   ├── gps.c               | parser for interfacing with the GPS module
│   ├── gps.h               |
│   ├── i2c.c               | i2c primitives
│   ├── i2c.h               |
│   ├── Makefile            | run to build
│   ├── oled_gps.c          | main application
│   ├── oled_gps.h          |
│   ├── printf.c            | printf library, int-to-char formatting
│   ├── printf.h            |
│   ├── ssd1306.c           | i2c commands for interfacing with the ssd1306 oled driver
│   └── ssd1306.h           |
└── tools                   |
    ├── algo.py             | testbed for some C code, also displays fonts embedded in font.h
    └── oled_gps.ino        | reference code from Arduino, ported to MSP430
```

Run `make` in `src`, then `make upload`. The MSP430G2553 is connected using a Launchpad.

Tested using standard msp430-gcc (4.6.3 20120301 mspgcc LTS 20120406 unpatched)
