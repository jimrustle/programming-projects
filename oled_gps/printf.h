
#include <stdint.h>

enum printf_mode {
    SERIAL,
    SSD_OLED
} printf_mode;

enum zero_padding {
    NONE,
    ONE,
    TWO
} zero_padding;

void my_putc(uint8_t);
void my_puts(char *);
void my_printf(char *, ...);
void serial_write_char(char);
void ssd1306_draw_char_1(char);

void my_printf(char *, ...);

