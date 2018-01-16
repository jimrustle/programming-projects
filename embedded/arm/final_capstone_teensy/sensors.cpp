
#include <Arduino.h>
#include "Adafruit_VL53L0X.h"
#include <NewPing.h>        // for the Ultrasound sensors

#include "sensors.h"
#include "pin_maps.h"

#define MAX_DISTANCE 500    // 500 cm
#define FILTER_ITERATIONS 50

/* define vl53 sensors */
Adafruit_VL53L0X vl53_left  = Adafruit_VL53L0X();
Adafruit_VL53L0X vl53_right = Adafruit_VL53L0X();

/* define ultrasonic sensors */
NewPing sonar_left(  PIN_TRIGL, PIN_ECHOL, MAX_DISTANCE);
NewPing sonar_right( PIN_TRIGR, PIN_ECHOR, MAX_DISTANCE);
NewPing sonar_centre(PIN_TRIGC, PIN_ECHOC, MAX_DISTANCE);


/* reads accelerometer, returns if fall detected
 * input: none
 * output: true if fall detected, otherwise false
 */
bool accelerometer_check(void) {
  static float old_val = 1000;
  float new_val = 0;

  for (int i = 0; i < FILTER_ITERATIONS; i++) {
    float x_mag = analogRead(PIN_ACCEL_X);
    float y_mag = analogRead(PIN_ACCEL_Y);
    float z_mag = analogRead(PIN_ACCEL_Z);
    float accel_mag = sqrt(x_mag * x_mag + y_mag * y_mag + z_mag * z_mag);
    new_val += 0.5 * (accel_mag - new_val);
  }

  // 194 at zero, 230 at 30°

  float diff = new_val - old_val;
  if (diff < 0) {
    diff = -diff;
  }
  old_val = new_val;

  //Serial.print("New_val: "); Serial.println(new_val);
  // Serial.print("Diff: "); Serial.println(diff);
  // Serial1.println(diff);
  if (diff > 50) {
    Serial.println(diff);
    Serial.println("FALL DETECTED");
    return true;
  }

  return false;
}

/* reads either the left or right Sharp sensor
 * input:   LEFT, RIGHT
 * output:  returns a float of the measured distance in centimetres
 */
int read_distance_sharp(direction_t direction) {
  int analogVal = 0;
  int sharp_pin = PIN_SHARP_VOR;

  if (direction == LEFT) {
    sharp_pin = PIN_SHARP_VOL;
  } else {
    sharp_pin = PIN_SHARP_VOR;
  }

  // sample and use a moving-window average filter to reduce noise
  for (int i = 0; i < FILTER_ITERATIONS; i++) {
    analogVal += (analogRead(sharp_pin) - analogVal) >> 1;
  }

  float aVal = analogVal;
  return 608365995.92 * pow(aVal, -2.3567);
}

/* reads the left, centre, or right ultrasound sensor
 * input:   LEFT, RIGHT, CENTRE
 * output:  returns an int of the measured distance in centimetres
 */
int read_distance_ultrasound(direction_t direction) {
  int distance = 0;
  switch (direction) {
  case LEFT:
    // for (int i = 0; i < FILTER_ITERATIONS; i++) {
    //   distance += (sonar_left.ping_cm() - distance) >> 1;
    // }
    distance = sonar_left.ping_cm();
    break;
  case RIGHT:
    // for (int i = 0; i < FILTER_ITERATIONS; i++) {
    //   distance += (sonar_right.ping_cm() - distance) >> 1;
    // }
    distance = sonar_right.ping_cm();
    break;
  case CENTRE:
    // for (int i = 0; i < FILTER_ITERATIONS; i++) {
    //   distance += (sonar_centre.ping_cm() - distance) >> 1;
    // }
    distance = sonar_centre.ping_cm();
    break;
  }

  return distance;
}

/* reads the distance information from the left or right VL53L0X sensor
 * input:   LEFT, RIGHT
 * output:  distance in centimetres
 */
int read_distance_vl53(direction_t direction) {
  VL53L0X_RangingMeasurementData_t measure;
  int distance = 0;

  switch (direction) {
  case LEFT:
    vl53_left.getSingleRangingMeasurement(&measure, false);
    break;
  case RIGHT:
    vl53_right.getSingleRangingMeasurement(&measure, false);
    break;
  case CENTRE: // should never be called, so stick it in a non-terminating loop
    Serial.println("Wrong call to VL53 - no CENTRE sensor");
    while (1);
    break;
  }

  if (measure.RangeStatus != 4) {
    distance = measure.RangeMilliMeter / 10.0;
  } else {
    // returns 0 if reading is out-of-range
    distance = 819;
  }

  return distance;
}

/* note: XSHDWN pins are active low - sensors are turned on using LOW
 * digitalWrites, and are turned off using HIGH digialWrites
 */
void remap_vl53_sensors(void) {
  // remap right sensor to new address
  digitalWrite(PIN_XSHDWN_RIGHT, HIGH);

  if (!vl53_right.begin()) {
    Serial.println("Failed to boot right VL53L0X");
    while(1);
  } else {
    Serial.println("Right sensor OK");
  }

  vl53_right.change_address(0x34);

  // turn on left sensor
  digitalWrite(PIN_XSHDWN_LEFT, HIGH);

  if (!vl53_left.begin()) {
    Serial.println("Failed to boot left VL53L0X");
    while(1);
  } else {
    Serial.println("Left sensor OK");
  }
}
