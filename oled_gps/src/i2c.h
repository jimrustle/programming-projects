#include<stdint.h>

void i2c_stop(void);
void i2c_start_tx(void);
void i2c_start_rx(void);
void i2c_write(uint8_t data);
uint8_t i2c_read_ack(void);
uint8_t i2c_read_nack(void);

