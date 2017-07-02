
#include <stdint.h>

typedef struct timestore_t {
    uint8_t hour, minute, second;
} timestore_t;

typedef enum prog_state_t {
    NO_DATA,
    GPS_FIX
} prog_state_t;

static prog_state_t program_state;

void TIMERA0_ISR(void);
void USCI0RX_ISR(void);
void serial_write_char(char byte);

