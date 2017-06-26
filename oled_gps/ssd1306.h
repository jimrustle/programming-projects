#include <stdint.h>
#include <stdlib.h>

#define SSD1306_SETCONTRAST 0x81
#define SSD1306_DISPLAYALLON_RESUME 0xA4
#define SSD1306_DISPLAYALLON 0xA5
#define SSD1306_NORMALDISPLAY 0xA6
#define SSD1306_INVERTDISPLAY 0xA7
#define SSD1306_DISPLAYOFF 0xAE
#define SSD1306_DISPLAYON 0xAF
#define SSD1306_SETDISPLAYOFFSET 0xD3
#define SSD1306_SETCOMPINS 0xDA
#define SSD1306_SETVCOMDETECT 0xDB
#define SSD1306_SETDISPLAYCLOCKDIV 0xD5
#define SSD1306_SETPRECHARGE 0xD9
#define SSD1306_SETMULTIPLEX 0xA8
#define SSD1306_SETLOWCOLUMN 0x00
#define SSD1306_SETHIGHCOLUMN 0x10
#define SSD1306_SETSTARTLINE 0x40
#define SSD1306_SETSTARTPAGE 0xB0
#define SSD1306_MEMORYMODE 0x20
#define SSD1306_COLUMNADDR 0x21
#define SSD1306_PAGEADDR   0x22
#define SSD1306_COMSCANINC 0xC0
#define SSD1306_COMSCANDEC 0xC8
#define SSD1306_SEGREMAP 0xA0
#define SSD1306_CHARGEPUMP 0x8D
#define SSD1306_SWITCHCAPVCC 0x2
#define SSD1306_NOP 0xE3

/* Write to Command register. */
#define SSD1306_MODE_CMD     0
/* Write one byte to display RAM. */
#define SSD1306_MODE_RAM     1
/* Write to display RAM with possible buffering. */
#define SSD1306_MODE_RAM_BUF 2

#define SSD1306_ADDR (0x3C)
#define SSD1306_WIDTH 128
#define SSD1306_HEIGHT 64

uint8_t cur_column;
uint8_t cur_row;
uint8_t text_size;

uint8_t char_lookup(char c, uint8_t col);
uint64_t scale_glyph_by_size(uint8_t glyph);

void ssd1306_write_display(uint8_t byte, uint8_t mode);
void ssd1306_write_command(uint8_t command);

void ssd1306_init(void);
void ssd1306_clear_screen(void);

void ssd1306_draw_string(const char* s, size_t len, uint8_t size);
void ssd1306_draw_int(int i, uint8_t size);
void ssd1306_draw_char(char c, uint8_t size);
void ssd1306_draw_char_1(char ch);

void ssd1306_cursor_to(uint8_t col, uint8_t page);

