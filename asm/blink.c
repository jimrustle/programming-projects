#include <msp430g2553.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

int main(void) {
    WDTCTL = WDTPW + WDTHOLD;

    // set 1 MHz for DCO, SMCLK is set to DCO
    BCSCTL1 = CALBC1_1MHZ;
    DCOCTL = CALDCO_1MHZ;

    // output LED for heartbeat
    P1DIR |= BIT0;
    P1OUT = BIT0;

    BCSCTL3 = LFXT1S_2;                 // select VLOCLK (12kHz) for ACLK
    TACCR0 = 12000;                     // 12000 * 1/12kHz = ~1 second
    TACCTL0 |= CCIE;
    TACTL = TASSEL_1 | MC_1;            // use ACLK, timer in continuous mode

    __bis_SR_register(GIE);
    while (true) {
        LPM3;
    }

    return 0;
}

/* 1-second interrupt timer */
#pragma vector=TIMER0_A0_VECTOR
__interrupt void TIMERA0_ISR(void) {
    P1OUT ^= BIT0;
}

