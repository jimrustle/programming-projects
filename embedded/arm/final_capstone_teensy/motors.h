
#include <stdint.h>

typedef enum motor_t { SFL, SNL, SC, SNR, SFR } motor_t;
uint8_t intensity_map(motor_t motor, int distance);
void buzz_motor(motor_t motor, int intensity);

