
#include "oled_gps.h"
#include <stdint.h>

#define DS3231_ADDR  (0x68)

void ds3231_set_time(void);
void ds3231_read_time(timestore_t* current_time);
//void ds3231_read_temp(uint8_t * temp);
uint8_t ds3231_read_temp(void);

