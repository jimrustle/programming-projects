#include <msp430g2553.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

#include "i2c.h"
#include "font.h"
#include "ssd1306.h"
#include "printf.h"

void ssd1306_write_display(uint8_t byte, uint8_t mode) {
    /*UCB0I2CSA = SSD1306_ADDR;*/
    i2c_start_tx();
    i2c_write(mode == SSD1306_MODE_CMD ? 0X00: 0X40);
    i2c_write(byte);
    i2c_stop();
}

void ssd1306_write_command(uint8_t command) {
    ssd1306_write_display(command, SSD1306_MODE_CMD);
}

void ssd1306_cursor_to(uint8_t col, uint8_t page) {
    cur_column = col;
    cur_row = page;

    i2c_start_tx();
    i2c_write(0x00);
    i2c_write(0xB0 | page);
    i2c_stop();

    i2c_start_tx();
    i2c_write(0x00);
    i2c_write(0x00 | (col & 0x0F));
    i2c_stop();

    i2c_start_tx();
    i2c_write(0x00);
    i2c_write(0x10 | (col & 0xF0) >> 4);
    i2c_stop();
}

void ssd1306_init(void) {
    UCB0I2CSA = SSD1306_ADDR;
    ssd1306_write_command(SSD1306_DISPLAYOFF);                    // 0xAE
    ssd1306_write_command(SSD1306_SETDISPLAYCLOCKDIV);            // 0xD5
    ssd1306_write_command(0x80);                                  // the suggested ratio 0x80

    ssd1306_write_command(SSD1306_SETMULTIPLEX);                  // 0xA8
    ssd1306_write_command(64 - 1);

    ssd1306_write_command(SSD1306_SETDISPLAYOFFSET);              // 0xD3
    ssd1306_write_command(0x0);                                   // no offset
    ssd1306_write_command(SSD1306_SETSTARTLINE | 0x0);            // line #0
    ssd1306_write_command(SSD1306_CHARGEPUMP);                    // 0x8D
    ssd1306_write_command(0x14);
    ssd1306_write_command(SSD1306_MEMORYMODE);                    // 0x20
    ssd1306_write_command(0x02);                                  // use page mode to draw characters
    ssd1306_write_command(SSD1306_SEGREMAP | 0x1);
    ssd1306_write_command(SSD1306_COMSCANDEC);

    ssd1306_write_command(SSD1306_SETCOMPINS);                    // 0xDA
    ssd1306_write_command(0x12);
    ssd1306_write_command(SSD1306_SETCONTRAST);                   // 0x81
    ssd1306_write_command(0xCF);


    ssd1306_write_command(SSD1306_SETPRECHARGE);                  // 0xd9
    ssd1306_write_command(0xF1);
    ssd1306_write_command(SSD1306_SETVCOMDETECT);                 // 0xDB
    ssd1306_write_command(0x40);
    ssd1306_write_command(SSD1306_DISPLAYALLON_RESUME);           // 0xA4
    ssd1306_write_command(SSD1306_NORMALDISPLAY);                 // 0xA6

    ssd1306_write_command(0x2E); // deactivate scroll

    ssd1306_write_command(SSD1306_DISPLAYON);//--turn on oled panel
}

void ssd1306_draw_string(const char* s, size_t len, uint8_t size) {
    for (size_t i = 0; i < len; i++) {
        ssd1306_draw_char(s[i], size);
    }
}

void ssd1306_clear_screen() {
    UCB0I2CSA = SSD1306_ADDR;
    for (uint8_t page = 0; page < 8; page++) {
        i2c_start_tx();
        i2c_write(0x00);
        i2c_write(0x10); // set column to zero
        i2c_stop();

        i2c_start_tx();
        i2c_write(0x00);
        i2c_write(0xB0 | page); // set page (row)
        i2c_stop();

        for (int i = 0; i < 128; i++) {
            i2c_start_tx();
            i2c_write(0x40);
            i2c_write(0x00);
            i2c_stop();
        }
    }
}

void ssd1306_draw_char_1(char ch) {
    ssd1306_draw_char(ch, text_size);
}

void ssd1306_draw_char(char ch, uint8_t size) {
    if (ch == '\n') {
        ssd1306_cursor_to(0, cur_row + 1);
        return; // don't draw anything to screen if \n
    }
    switch (size) {
        // size of 2 uses a lookup table, while size of 3
        // calculates the scaled glyph -- FIXME? - generalize scaling
        case 5:
        case 4:
        case 3:
            {
                for (uint8_t i = 0; i < size; i++) {
                    for (uint8_t j = 0; j < 5; j++) {
                        uint8_t c = char_lookup(ch, j);
                        uint64_t glyph = scale_glyph_by_size(c);

                        i2c_start_tx();
                        i2c_write(0x40);
                        for (uint8_t k = 0; k < size; k++) {
                            i2c_write((glyph & (0xFFLL << i * 8)) >> i * 8);
                        }
                        i2c_stop();
                    }
                    ssd1306_cursor_to(cur_column, cur_row + 1);
                }

                /*for (uint8_t j = 0; j < 5; j++) {*/
                    /*uint8_t c = char_lookup(ch, j);*/
                    /*uint32_t glyph = scale_glyph_by_size(c);*/

                    /*i2c_start_tx();*/
                    /*i2c_write(0x40);*/
                    /*i2c_write(glyph & 0x00FF);*/
                    /*i2c_write(glyph & 0x00FF);*/
                    /*i2c_write(glyph & 0x00FF);*/
                    /*i2c_stop();*/
                /*}*/

                /*ssd1306_cursor_to(cur_column, cur_row + 1);*/

                /*for (uint8_t j = 0; j < 5; j++) {*/
                    /*uint8_t c = char_lookup(ch, j);*/
                    /*uint32_t glyph = scale_glyph_by_size(c);*/

                    /*i2c_start_tx();*/
                    /*i2c_write(0x40);*/
                    /*i2c_write((glyph & 0x00FF << 8) >> 8);*/
                    /*i2c_write((glyph & 0x00FF << 8) >> 8);*/
                    /*i2c_write((glyph & 0x00FF << 8) >> 8);*/
                    /*i2c_stop();*/
                /*}*/

                /*ssd1306_cursor_to(cur_column, cur_row + 1);*/

                /*for (uint8_t j = 0; j < 5; j++) {*/
                    /*uint8_t c = char_lookup(ch, j);*/
                    /*uint32_t glyph = scale_glyph_by_size(c);*/

                    /*i2c_start_tx();*/
                    /*i2c_write(0x40);*/
                    /*i2c_write((glyph & (0xFFL << 16)) >> 16);*/
                    /*i2c_write((glyph & (0xFFL << 16)) >> 16);*/
                    /*i2c_write((glyph & (0xFFL << 16)) >> 16);*/
                    /*i2c_stop();*/
                /*}*/
                // 16 = 3 * glyph_with + 1 kerning
                /*ssd1306_cursor_to(cur_column + 17, cur_row - size + 1);*/
                ssd1306_cursor_to(cur_column + 5*size + 2, cur_row - size);
                break;
            }
        case 2:
            {
                // for column in glyph
                for (uint8_t j = 0; j < 5; j++) {
                    // get column and lower nibble
                    uint8_t c = char_lookup(ch, j);
                    uint8_t top = (c & 0x0F);
                    i2c_start_tx();
                    i2c_write(0x40);
                    i2c_write(scale_two[top]);
                    i2c_write(scale_two[top]);
                    i2c_stop();
                }

                // move down a page to complete the
                // bottom half of the glyph
                ssd1306_cursor_to(cur_column, cur_row + 1);

                for (uint8_t j = 0; j < 5; j++) {
                    uint8_t c = char_lookup(ch, j);
                    uint8_t bot = (c & 0xF0) >> 4;

                    if (size == 2) {
                        i2c_start_tx();
                        i2c_write(0x40);
                        i2c_write(scale_two[bot]);
                        i2c_write(scale_two[bot]);
                        i2c_stop();
                    }
                }

                // add width of $size characters and kerning,
                // return to original row
                ssd1306_cursor_to(cur_column + 5*size + 1, cur_row - 1);
                break;
            }
        case 1:
        default:
            i2c_start_tx();
            i2c_write(0x40);
            for (uint8_t j = 0; j < 5; j++) {
                i2c_write(char_lookup(ch, j));
            }
            i2c_write(0x00); // add kerning
            i2c_stop();
    }
}

/* helper functions for ssd commands */

uint8_t char_lookup(char c, uint8_t col) {
    // if digit, convert to char
    if ((0 <= c) && (c <= 9)) {
        c += 48;
    }
    // chars are 5x7, so check for cols < 5
    return ((' ' <= c) && (c < '~') && col < 5) ? font[5*(c - ' ') + col] : 0x00;
}

uint64_t scale_glyph_by_size(uint8_t glyph) {
    uint64_t res = 0;
    uint8_t top = (glyph & 0xF0) >> 4;
    for (int8_t k = 3; k >= 0; k--) {
        for (uint8_t j = 0; j < text_size; j++) {
        res |= ((top >> k) & 1);
        res <<= 1;
        }
    }

    uint8_t bot = (glyph & 0x0F);
    for (int8_t k = 3; k >= 0; k--) {
        for (uint8_t j = 0; j < text_size; j++) {
        res |= ((bot >> k) & 1);
        res <<= 1;
        }
    }

    // $res is now a 32-bit that holds a
    // scaled column of a glyph
    res >>= 1;

    return res;
}
