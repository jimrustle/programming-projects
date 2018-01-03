#include <msp430g2553.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>

#include "printf.h"
#include "i2c.h"
#include "ds3231.h"
#include "ssd1306.h"
#include "font.h"
#include "gps.h"

volatile bool gps_fix = false;

void serial_write_char(char b) {
    while (!(IFG2 & UCA0TXIFG));
    UCA0TXBUF = (unsigned char) b;
}

int main(void) {
    WDTCTL = WDTPW + WDTHOLD;

    // set 1 MHz for DCO, SMCLK is set to DCO
    BCSCTL1 = CALBC1_1MHZ;
    DCOCTL = CALDCO_1MHZ;

    // configure UART
    UCA0CTL1 |= UCSWRST;            // hold UCSIA module in reset
    UCA0CTL1 |= UCSSEL1;            // select ACLK
    UCA0BR1 = 0;                    // page 425, table 15-5 : 9600 baud, 1MHz
    UCA0BR0 = 6;                    // page 425, table 15-5 : 9600 baud, 1MHz
    UCA0MCTL |= UCBRF_8 | UCOS16;   // page 425, table 15-5, enable oversampling

    P1SEL  = BIT1 + BIT2 + BIT6 + BIT7; // set ports to UC functions
    P1SEL2 = BIT1 + BIT2 + BIT6 + BIT7; // (UCA = serial, UCB = I2C)

    UCA0CTL1 &= ~UCSWRST;           // activate UCSIA module

    // configure I2C
    UCB0CTL1 |= UCSWRST;            // hold UCSIB module in reset
    UCB0CTL0 |= UCMODE_3 + UCMST + UCSYNC;
    UCB0CTL1 |= UCSSEL_2 + UCSWRST;
    UCB0BR0 = 10;                   // 1 MHz / 10 = 100 kHz
    UCB0BR1 = 0;
    UCB0I2CSA = DS3231_ADDR;

    P1SEL  = BIT1 + BIT2 + BIT6 + BIT7; // set ports to UC functions
    P1SEL2 = BIT1 + BIT2 + BIT6 + BIT7; // (UCA = serial, UCB = I2C)

    UCB0CTL1 &= ~UCSWRST;               // activate UCSIB module

    UC0IE |= UCA0RXIE;                  // enable UCA0RX interrupt, UCB0TX interrupt

    // output LED for heartbeat
    P1DIR |= BIT0;
    P1OUT = BIT0;

    BCSCTL3 = LFXT1S_2;                 // select VLOCLK (12kHz) for ACLK
    TACCR0 = 16000;                     // 12000 * 1/12kHz = ~1 second
    TACCTL0 |= CCIE;
    TACTL = TASSEL_1 | MC_1;            // use ACLK, timer in continuous mode

    // delay for ssd1306 display to power up and accept i2c commands
    for (int i = 0; i < 200; i++) {
        __delay_cycles(1000);
    }

#ifdef SET_TIME
    ds3231_set_time();
#endif

    ssd1306_init();
    ssd1306_cursor_to(0, 0);
    ssd1306_clear_screen();
    ssd1306_cursor_to(0, 3);
    printf_mode = SSD_OLED;
    text_size = 3;
    my_printf("No data");

    UCB0I2CSA = DS3231_ADDR;
    __bis_SR_register(GIE);

    program_state = NO_DATA;

    while (true) {
        LPM3;
    }

    return 0;
}

/* ucsi A0 receive interrupt vector */
#pragma vector=USCIAB0RX_VECTOR
__interrupt void USCI0RX_ISR(void) {
    uint8_t c = UCA0RXBUF;
    /*serial_write_char((char) c);    // echo received char*/
    parse_GPS((char) c);
    /*P1OUT ^= BIT0;                  // blink heartbeat LED*/
}

/* 1-second interrupt timer */
#pragma vector=TIMER0_A0_VECTOR
__interrupt void TIMERA0_ISR(void) {
    static uint8_t temp;
    static timestore_t time = {0, 0, 0};
    P1OUT ^= BIT0;

    UCB0I2CSA = DS3231_ADDR;

    ds3231_read_time(&time);
    temp = ds3231_read_temp();

    UCB0I2CSA = SSD1306_ADDR;

    switch (program_state) {
        case NO_DATA:
            if (gps_fix) {
                program_state = GPS_FIX;

                ssd1306_clear_screen();
                ssd1306_cursor_to(0, 0);
            }
            else {
                zero_padding = ONE;
                ssd1306_cursor_to(20, 0);
                text_size = 2;
                my_printf("%d:%d:%d\n", time.hour, time.minute, time.second);
            }
            break;
        case GPS_FIX:
            if (gps_fix) {
                gps_fix = 0;

                printf_mode = SSD_OLED;
                zero_padding = ONE;

                // print time and date, from both GPS and DS3231
                text_size = 1;
                ssd1306_cursor_to(0, 0);
                my_printf("%d:%d:%d - %d\n", gps_time/100,
                        10*((gps_time/10)%10) + gps_time % 10, gps_csecs/100,
                        gps_date);
                my_printf("%d:%d:%d\n", time.hour, time.minute, time.second);

                // print speed in HUGE, in centre
                zero_padding = TWO;

                text_size = 5;
                /*ssd1306_cursor_to(0, 3);*/
                /*my_printf("   "); // clear old value*/
                ssd1306_cursor_to(0, 3);
                my_printf("%d", (int) (gps_knots/100.0 * 1.852));

                // print km/h unit in bottom-right corner
                zero_padding = ONE;
                text_size = 1;
                ssd1306_cursor_to(104, 7);
                my_printf("km/h");

                // print temperature in upper-right corner
                text_size = 1;
                ssd1306_cursor_to(104, 0);
                my_printf("%d@C\n", (temp & 0x7F));
            }
            else {
                program_state = NO_DATA;

                ssd1306_clear_screen();
                ssd1306_cursor_to(0, 3);
                printf_mode = SSD_OLED;
                text_size = 3;
                my_printf("No data");
            }
            break;
        default:
            break;
    }
}

