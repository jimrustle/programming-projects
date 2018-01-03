
#include <stdint.h>
#include "i2c.h"
#include "ds3231.h"

#define HEX(n) 0x##n
#define HEXIFY(n) HEX(n)

void ds3231_set_time(void) {
    i2c_start_tx();
    i2c_write(0x00);
#ifdef SET_TIME
    i2c_write(HEXIFY(COMPILE_SEC_D));
    i2c_write(HEXIFY(COMPILE_MIN_D));
    i2c_write(HEXIFY(COMPILE_HOUR_D));
    i2c_write(HEXIFY(COMPILE_DAY_OF_WEEK_D));
    i2c_write(HEXIFY(COMPILE_DAY_OF_MONTH_D));
    i2c_write(HEXIFY(COMPILE_MONTH_OF_YEAR_D));
    i2c_write(HEXIFY(COMPILE_YEAR_D));
#endif
    i2c_stop();
}

void ds3231_read_time(timestore_t* current_time) {
    uint8_t hour, minute, second;

    i2c_start_tx();
    i2c_write(0x00);

    i2c_start_rx();
    second = i2c_read_ack();
    current_time->second = (uint8_t) ((second & 0xF0) >> 4) * 10 + (second & 0x0F);

    minute = i2c_read_ack();
    current_time->minute = (uint8_t) ((minute & 0xF0) >> 4) * 10 + (minute & 0x0F);

    hour = i2c_read_nack();
    current_time->hour = (uint8_t) ((hour & 0x30) >> 4) * 10 + (hour & 0x0F);
}

uint8_t ds3231_read_temp(void) {
    i2c_start_tx();
    i2c_write(0x11);
    i2c_start_rx();
    uint8_t temp = i2c_read_nack();

    return temp;
}
