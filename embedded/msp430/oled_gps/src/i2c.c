#include <msp430g2553.h>
#include "i2c.h"

void i2c_stop(void) {
    UCB0CTL1 |= UCTXSTP;
    while (UCB0CTL1 & UCTXSTP);
}

void i2c_start_tx(void) {
    while (UCB0CTL1 & UCTXSTP);
    UCB0CTL1 |= UCTR + UCTXSTT;
}

void i2c_start_rx(void) {
    UCB0CTL1 &= ~UCTR;
    UCB0CTL1 |= UCTXSTT;
    while (UCB0CTL1 & UCTXSTT);
}

void i2c_write(uint8_t data) {
    UCB0TXBUF = data;
    while(!(IFG2 & UCB0TXIFG));
}

uint8_t i2c_read_ack(void) {
    while (!(UC0IFG & UCB0RXIFG)) ;
    return UCB0RXBUF;
}

uint8_t i2c_read_nack(void) {
    uint8_t ret = i2c_read_ack();
    i2c_stop();
    i2c_read_ack();
    return ret;
}

