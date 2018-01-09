
#include <msp430.h>
#include <msp430g2553.h>

#define PC r0
#define SP r1
#define SR r2
#define CG r3

.section ".bss"  ; uninitialized variables
.section ".data" ; initialized variables, copied from flash into ram
.section ".text"

.global main

#define LED_GREEN 64
#define LED_RED 1

.macro led_on color
    bis.b \color, &P1OUT
.endm

.macro led_off color
    bic.b \color, &P1OUT
.endm

reset:
    ; set stack pointer: the start of ram/.bss plus 512 bytes
    mov.w #0x400, SP

main:
    ; disable watchdog
    mov.w #WDTPW+WDTHOLD, &WDTCTL

    ; configure 1MHz clock
    ; calibrated values are stored in the infomem segment A,
    ; so we grab the values and store them in the configuration registers for the DCO
    mov.b &CALBC1_1MHZ_, &BCSCTL1 ; set RSELx
    mov.b &CALDCO_1MHZ_, &DCOCTL  ; set DCOx and MODx

    ; set up port IO
    ; set P1.0 and P1.6 to output
    bis.b #LED_RED+LED_GREEN, &P1DIR
    ; turn on the LEDs
    bis.b #LED_RED+LED_GREEN, &P1OUT

    ; configure ACLK for 1 Hz clock
    mov.b #LFXT1S_2, &BCSCTL3    ; select VLOCLK (12 kHz) for ACLK
    mov.w #2400, &TACCR0         ; compare @ 12000 -> 1/12 kHz * 12000 = ~ 1 second
    bis.b #CCIE, &TACCTL0        ; CCIE enable
    mov.w #MC_1+TASSEL_1, &TACTL ; use ACLK in continuous mode

    ; configure UART @ 9600 baud - see pg 425 table 15-5 in slau144
    bis.b #UCSWRST, &UCA0CTL1          ; hold UCSIA in reset mode
    bis.b #UCSSEL1, &UCA0CTL1          ; use ACLK as timer source
    mov.b #0, &UCA0BR1                 ; 9600 baud @ 1 MHz
    mov.b #6, &UCA0BR0                 ; 9600 baud @ 1 MHz
    bis.b #UCBRF_8 + UCOS16, &UCA0MCTL ; enable oversampling
    bic.b #UCSWRST, &UCA0CTL1          ; remove from reset mode

    ; set pins to alternate function - UART ports
    bis.b #BIT1+BIT2, &P1SEL
    bis.b #BIT1+BIT2, &P1SEL2

    ; enable UART interrupt
    bis.b #UC0IE, &UCA0RXIE

    ; clear r14
    clr r14
    ; sleep and enable interrupts
sleep:
    bis #GIE+CPUOFF+SCG1+SCG0, SR
    jmp sleep

; in:  r13
; out: none
send_char:
    ; wait until TX buffer empty
    bit.b #UCA0TXIFG, &IFG2
    jz send_char
    ; send whatever is in r13
    mov.b r13, &UCA0TXBUF
    ret

; timer interrupt
TIMER_A0_VECTOR:
    xor.b #LED_RED, &P1OUT
    tst r14
    jz timer_a0_exit
    xor.b #LED_GREEN, &P1OUT
    dec.b r14
timer_a0_exit:
    reti

; UART interrupt
USCI_A0_VECTOR:
    xor.b #LED_GREEN, &P1OUT
    ; drop char if it is not numeric
    mov &UCA0RXBUF, r15 ; read from uart rx buf
    ; echo to screen
    mov.b r15, r13
    call #send_char
    ; check if ascii numeral
    sub.b #48, r15      ; subtract '0'
    jn usci_exit        ; if negative, it's not a number
    jz usci_exit        ; skip zero
    cmp #10, r15        ; if greater than '9', it's not a number either
    jhs usci_exit
    ; save value for later
    mov.b r15, r14

    led_off #LED_GREEN
usci_exit:
    reti

int_unused:
    jmp int_unused

; vectors from 0xffc0 to 0xffde are not used
.section ".vectors", "ax", @progbits
    .word int_unused      ;0xffe0 not used
    .word int_unused      ;0xffe2 not used
    .word int_unused      ;0xffe4 IO Port P1
    .word int_unused      ;0xffe6 IO Port P2
    .word int_unused      ;0xffe8 not used
    .word int_unused      ;0xffea ADC10 ADC10IFG
    .word int_unused      ;0xffec USCI_A0/USCI_B0 transmit, USCI_B0 I2C rx/tx, UC{A/B}0TXIFG
    .word USCI_A0_VECTOR  ;0xffee USCI_A0/USCI_B0 receive, USCI_B0 I2C status, UC{A/B}0RXIFG
    .word int_unused      ;0xfff0 Timer0_A3 TA0CCR{1,2} CCIFG, TAIFG
    .word TIMER_A0_VECTOR ;0xfff2 Timer0_A3 TA0CCR0 CCIFG
    .word int_unused      ;0xfff4 Watchdog WDTIFG
    .word int_unused      ;0xfff6 Comparator_A+ CAIFG
    .word int_unused      ;0xfff8 Timer1_A3 TA1CCR{1,2} CCIFG, TAIFG
    .word int_unused      ;0xfffa Timer1_A3 (TA1CCR0 CCIFG)
    .word int_unused      ;0xfffc NMI, oscillator fault, or flashmem access violation
    .word reset           ;0xfffe Reset
.end
