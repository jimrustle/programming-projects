
# babby's MSP430 assembly template, with UART

This program reads a character from the UART port, and determines if it is an integer ('0' to '9'), then blinks the LED that many times.

# Usage

Run `make` to compile your .S file.

Run `make upload` to upload to the MSP430 launchpad.

# Toolchain

Developed using: ``msp430-elf-gcc (SOMNIUM Technologies Limited - msp430-gcc 6.4.0.32) 6.4.0``

MSP430 Launchpad, with MSP430G2553, make sure you set the `TXD` and `RXD` jumpers horizontally to hardware mode.


