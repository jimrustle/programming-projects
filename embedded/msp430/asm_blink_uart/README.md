
# babby's MSP430 assembly template, with UART

This program reads a character from the UART port, and determines if it is an integer ('0' to '9'), then blinks the LED that many times.

# Usage

Run `make` to compile your .S file.

Run `make upload` to upload to the MSP430 launchpad.

# Toolchain

Developed using: ``msp430-gcc (GCC) 4.6.3 20120301 (mspgcc LTS 20120406 unpatched)``

MSP430 Launchpad, with MSP430G2553, make sure you set the `TXD` and `RXD` jumpers horizontally to hardware mode.


