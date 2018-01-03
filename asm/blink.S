
#include <msp430.h>
#include <msp430g2553.h>

#define PC r0
#define SP r1
#define SR r2
#define CG r3

.section ".bss"                   ; uninitialized variables
.section ".data"                  ; initialized variables, copied from flash into ram
.section ".text"

.global main

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
    ; set P1.0 to output
    bis.b #1, &P1DIR
    ; turn on the LED
    bis.b #1, &P1OUT

    ; configure ACLK for 1 Hz clock
    mov.b #LFXT1S_2, &BCSCTL3     ; select VLOCLK (12 kHz) for ACLK
    mov.w #12000, &TACCR0         ; compare @ 12000 -> 1/12 kHz * 12000 = ~ 1 second
    bis.b #CCIE, &TACCTL0         ; CCIE enable
    mov.w #MC_1+TASSEL_1, &TACTL  ; use ACLK in continuous mode

; sleep and enable interrupts
    eint
sleep:
    bis #CPUOFF+SCG1+SCG0, SR
    jmp sleep

; timer interrupt
TIMER_A0_VECTOR:
    xor.b #1, &P1OUT
    reti

int_unused:
    jmp int_unused

; vectors from 0xffc0 to 0xffde are not used
.section ".vectors", "ax", @progbits
    .word int_unused              ;0xffe0 not used
    .word int_unused              ;0xffe2 not used
    .word int_unused              ;0xffe4 IO Port P1
    .word int_unused              ;0xffe6 IO Port P2
    .word int_unused              ;0xffe8 not used
    .word int_unused              ;0xffea ADC10 ADC10IFG
    .word int_unused              ;0xffec USCI_A0/USCI_B0 transmit, USCI_B0 I2C rx/tx, UC{A/B}0TXIFG
    .word int_unused              ;0xffee USCI_A0/USCI_B0 receive, USCI_B0 I2C status, UC{A/B}0RXIFG
    .word int_unused              ;0xfff0 Timer0_A3 TA0CCR{1,2} CCIFG, TAIFG
    .word TIMER_A0_VECTOR         ;0xfff2 Timer0_A3 TA0CCR0 CCIFG
    .word int_unused              ;0xfff4 Watchdog WDTIFG
    .word int_unused              ;0xfff6 Comparator_A+ CAIFG
    .word int_unused              ;0xfff8 Timer1_A3 TA1CCR{1,2} CCIFG, TAIFG
    .word int_unused              ;0xfffa Timer1_A3 (TA1CCR0 CCIFG)
    .word int_unused              ;0xfffc NMI, oscillator fault, or flashmem access violation
    .word reset                   ;0xfffe Reset
.end
