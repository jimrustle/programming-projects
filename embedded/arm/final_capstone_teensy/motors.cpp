
#include "motors.h"
#include "pin_maps.h"
#include <Arduino.h>

uint8_t intensity_map(motor_t motor, int distance) {
  uint8_t intensity = 0;

  const int*     boundary_map;
  const uint8_t* intensity_map;

  static const int     sf_boundaries[]  = {5, 25, 60, 95, 130, 165, 200};
  static const uint8_t sf_intensities[] = {255, 235, 210, 180, 150, 128};
  // static const int     sn_boundaries[]  = {10, 100, 150, 200, 250, 300, 500};
  static const int     sn_boundaries[]  = {10, 25, 40, 55, 70, 85, 100};
  static const uint8_t sn_intensities[] = {255, 240, 235, 180, 150, 128};
  static const int     sc_boundaries[]  = {10, 50, 100, 150, 200, 250, 300};
  static const uint8_t sc_intensities[] = {255, 235, 230, 200, 150, 128};

  switch (motor) {
    /* if ultrasound SFL < 2 m, buzz */
    case SFR:
    case SFL: {
      boundary_map  = sf_boundaries;
      intensity_map = sf_intensities;
      break;
    }
    case SNR:
    case SNL: {
      boundary_map  = sn_boundaries;
      intensity_map = sn_intensities;
      break;
    }
    case SC: {
      boundary_map  = sc_boundaries;
      intensity_map = sc_intensities;
      break;
    }
  }

  for (int i = 0; i < 6; i++) {
    if ((boundary_map[i] <= distance) && (distance <= boundary_map[i+1])) {
      intensity = intensity_map[i];
    }
  }

  return intensity;
}

/* vibrates a motor with a certain intensity
 * input:   motor type (SFL, SNL, SC, SNR, SFR), intensity
 * output:  none
 */
uint8_t motor_list[] = {PIN_VIB1, PIN_VIB2, PIN_VIB3, PIN_VIB4, PIN_VIB5};
void buzz_motor(motor_t motor, int intensity) {
  analogWrite(motor_list[motor], intensity);
}
