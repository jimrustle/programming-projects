
# babby's MSP430 assembly template

A simple template for assembly development on the MSP430.

The template - `blink.S` - blinks an LED (P1.0 on the Launchpad) at a rate of 1 Hz. There is a functionally-equivalent C version of the code in `blink.c`.

# Usage 

Run `make` to compile your .S file.

Run `make compare` to compile the assembly version and C version, and dump the binary sizes.

# Toolchain

Developed using: ``msp430-gcc (GCC) 4.6.3 20120301 (mspgcc LTS 20120406 unpatched)``



