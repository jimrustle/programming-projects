/******************************************************************************
 *                          Reusable MSP430 printf()
 *
 * Description: This printf function was written by oPossum and originally
 *              posted on the 43oh.com forums. For more information on this
 *              code, please see the link below.
 *
 *              http://www.43oh.com/forum/viewtopic.php?f=10&t=1732
 *
 *              A big thanks to oPossum for sharing such great code!
 *
 * Author:  oPossum
 * Source:  http://www.43oh.com/forum/viewtopic.php?f=10&t=1732
 * Date:    10-17-11
 *
 * Note: This comment section was written by Nicholas J. Conn on 06-07-2012
 *       for use on NJC's MSP430 LaunchPad Blog.
 *
 *
 * 2017-06-25:
 * This code has additionally been modified to support:
 *  - writing to the serial port, or to an SSD 1306 OLED display
 *  - a single left-padded zero in %d formatting
 *
 ******************************************************************************/

#include "stdarg.h"
#include "printf.h"
#include "stdbool.h"

void my_puts(char *s) {
    char c;
    // Loops through each character in string 's'
    while ((c = *s++)) {
        if (printf_mode == SERIAL) {
            serial_write_char(c);
        } else {
            ssd1306_draw_char_1(c);
        }
    }
}

void my_putc(uint8_t b) {
    if (printf_mode == SERIAL) {
        serial_write_char((char) b);
    } else {
        ssd1306_draw_char_1((char) b);
    }
}

static const unsigned long dv[] = {
    //  4294967296      // 32 bit unsigned max
    1000000000,// +0
    100000000, // +1
    10000000, // +2
    1000000, // +3
    100000, // +4
    //       65535      // 16 bit unsigned max
    10000, // +5
    1000, // +6
    100, // +7
    10, // +8
    1, // +9
};

static void xtoa(unsigned long x, const unsigned long *dp) {
    char c;
    unsigned long d;
    bool lt_100 = false;
    bool lt_10 = false;

    if (x < 100) {
        lt_100 = true;
        if (x < 10) {
            lt_10 = true;
        }
    }

    if (x) {
        while (x < *dp) {
            ++dp;
        }
        do {
            d = *dp++;
            c = '0';
            while (x >= d) {
                ++c, x -= d;
            }
            if (zero_padding == TWO) {
                if (lt_100) {
                    my_putc((uint8_t) '0');
                }
                if (lt_10) {
                    my_putc((uint8_t) '0');
                }
            }
            if (zero_padding == ONE) {
                if (lt_10) {
                    my_putc((uint8_t) '0');
                }
            }
            my_putc((uint8_t) c);
        } while (!(d & 1));
    } else {
        if (zero_padding == TWO) {
            my_putc('0');
            my_putc('0');
        }
        if (zero_padding == ONE) {
            my_putc('0');
        }
        my_putc('0');
    }
}

static void my_puth(unsigned n) {
    static const char hex[16] = { '0', '1', '2', '3', '4', '5', '6', '7', '8',
        '9', 'A', 'B', 'C', 'D', 'E', 'F' };
        my_putc((uint8_t) hex[n & 15]);
}

void my_printf(char *format, ...)
{
    char c;
    int i;

    va_list a;
    va_start(a, format);
    while((c = *format++)) {
        if(c == '%') {
            switch(c = *format++) {
                case 's': // String
                    my_puts(va_arg(a, char*));
                    break;
                case 'c':// Char
                    my_putc((uint8_t) va_arg(a, int));
                    break;
                case 'd':// 16 bit Integer
                case 'i':// 16 bit Integer
                case 'u':// 16 bit Unsigned
                    i = va_arg(a, int);
                    if (c == 'i' && i < 0) i = -i, my_putc('-');
                    xtoa((unsigned) i, dv + 5);
                    break;
                case 'x':// 16 bit heXadecimal
                    i = va_arg(a, int);
                    my_puth((uint8_t) i >> 12);
                    my_puth((uint8_t) i >> 8);
                    my_puth((uint8_t) i >> 4);
                    my_puth((uint8_t) i);
                    break;
                case 0: return;
                default: goto bad_fmt;
            }
        } else
            bad_fmt: my_putc((uint8_t) c);
    }
    va_end(a);
}

