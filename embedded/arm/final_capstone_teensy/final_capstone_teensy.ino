/* Teensy code for bathat capstone

   [VIB1] [VIB2] [VIB3] [VIB4] [VIB5]
   SFL    SNL    SC     SNR    SFR

*/

#include "sensors.h"
#include "motors.h"
#include "pin_maps.h"

void setup() {
  Serial1.begin(9600);
  /*pinMode(PIN_UART_Rx_to_Tx, OUTPUT);*/
  /*pinMode(PIN_UART_Tx_to_Rx, OUTPUT);*/
  pinMode(PIN_SDA, OUTPUT);
  pinMode(PIN_SCL, OUTPUT);

  pinMode(PIN_VIB1, OUTPUT);
  pinMode(PIN_VIB2, OUTPUT);
  pinMode(PIN_VIB3, OUTPUT);
  pinMode(PIN_VIB4, OUTPUT);
  pinMode(PIN_VIB5, OUTPUT);

  /* these don't need to be set */
  /*pinMode(PIN_ECHOL, INPUT);*/
  /*pinMode(PIN_ECHOC, INPUT);*/
  /*pinMode(PIN_ECHOR, INPUT);*/
  /*pinMode(PIN_TRIGL, OUTPUT);*/
  /*pinMode(PIN_TRIGC, OUTPUT);*/
  /*pinMode(PIN_TRIGR, OUTPUT);*/

  pinMode(PIN_SHARP_VOL, INPUT);
  pinMode(PIN_SHARP_VOR, INPUT);
  pinMode(PIN_BUTTON, INPUT);

  pinMode(PIN_ACCEL_X, INPUT);
  pinMode(PIN_ACCEL_Y, INPUT);
  pinMode(PIN_ACCEL_Z, INPUT);
  pinMode(PIN_ACCEL_0G, OUTPUT);
  pinMode(PIN_ACCEL_GSEL, OUTPUT);
  /* pinMode(13, OUTPUT); */

  remap_vl53_sensors();

  accelerometer_check();
  delay(2000);
  accelerometer_check();
}

typedef enum program_state_t {
  RANGING, EMERGENCY_CALL, FALL_DETECTED, EMERGENCY_CALL_IMMEDIATE
} program_state_t;

typedef enum button_state_t {
  PRESSED, NOT_PRESSED
} button_state_t;

typedef enum vibrate_state_t {
  OFF, ON
} vibrate_state_t;

typedef enum range_state_t {
  US_FLR, VL_NLR, USSharpLidar_C,
} range_state_t;

void loop() {
  /* buzz_motor(SFL, 255); */
  /* delay(1000); */
  /* buzz_motor(SFL, 0); */
  /* buzz_motor(SNL, 255); */
  /* delay(1000); */
  /* buzz_motor(SNL, 0); */
  /* buzz_motor(SC, 255); */
  /* delay(1000); */
  /* buzz_motor(SC, 0); */
  /* buzz_motor(SNR, 255); */
  /* delay(1000); */
  /* buzz_motor(SNR, 0); */
  /* buzz_motor(SFR, 255); */
  /* delay(1000); */
  /* buzz_motor(SFR, 0); */

  long current_millis = millis();
  static long previous_millis = 0;
  static long button_down_millis = 0;
  static long button_up_millis = 0;
  static long previous_vib_millis = 0;
  static long previous_led_millis = 0;
  static long button_diff = 0;
  static int led_state = LOW;
  static program_state_t program_state = RANGING;
  static button_state_t button_state = NOT_PRESSED;
  static vibrate_state_t vibrate_state = OFF;
  static range_state_t ranging_state = US_FLR;
  static int debug_count = 0;

  /* Serial.print("Current: "); Serial.println(current_millis); */
  /* Serial.print("Prev: "); Serial.println(previous_millis); */

  if (program_state == RANGING) {
    /* Serial.println("mode: ranging"); */
    /* // test code for blinking the teensy's onboard LED: */
    /* if ((current_millis - previous_led_millis) > 500) { */
    /*   previous_led_millis = current_millis; */
    /*   if (led_state == LOW) { */
    /*     led_state = HIGH; */
    /*   } else { */
    /*     led_state = LOW; */
    /*   } */
    /* digitalWrite(13, led_state); */
    /* } */
    static int vl53_left_distance = 0;
    static int vl53_right_distance = 0;
    static int ultrasound_left_distance = 0;
    static int ultrasound_right_distance = 0;
    static int ultrasound_centre_distance = 0;
    static int sharp_left_distance = 0;
    static int sharp_right_distance = 0;

    ultrasound_left_distance = read_distance_ultrasound(LEFT);
    ultrasound_right_distance = read_distance_ultrasound(RIGHT);
    if (ultrasound_left_distance < 200) {
      buzz_motor(SFL, intensity_map(SFL, ultrasound_left_distance));
    } else {
      buzz_motor(SFL, 0);
    }

    if (ultrasound_right_distance < 200) {
      buzz_motor(SFR, intensity_map(SFR, ultrasound_right_distance));
    } else {
      buzz_motor(SFR, 0);
    }

    vl53_left_distance = read_distance_vl53(LEFT);
    vl53_right_distance = read_distance_vl53(RIGHT);
    if (vl53_left_distance < 100) {
      buzz_motor(SNL, intensity_map(SNL, vl53_left_distance));
    } else {
      buzz_motor(SNL, 0);
    }

    if (vl53_right_distance < 100) {
      buzz_motor(SNR, intensity_map(SNR, vl53_right_distance));
    } else {
      buzz_motor(SNR, 0);
    }
    ultrasound_centre_distance = read_distance_ultrasound(CENTRE);
    sharp_left_distance = read_distance_sharp(LEFT);
    sharp_right_distance = read_distance_sharp(RIGHT);
    if (ultrasound_centre_distance > 100) {
      buzz_motor(SC, intensity_map(SC, (sharp_right_distance + sharp_left_distance)/2.0));
    } else {
      buzz_motor(SC, intensity_map(SC, ultrasound_centre_distance));
      /* Serial.print(ultrasound_centre_distance); Serial.println(intensity_map(SC, ultrasound_centre_distance)); */
    }
  
    /* static int vl53_left_distance_last = 0; */
    /* static int vl53_right_distance_last = 0; */
    /* static int ultrasound_left_distance_last = 0; */
    /* static int ultrasound_right_distance_last = 0; */
    /* static int ultrasound_centre_distance_last = 0; */
    /* static int sharp_left_distance_last = 0; */
    /* static int sharp_right_distance_last = 0; */

    /* switch (ranging_state) { */
    /* case (US_FLR): { */
    /*   ultrasound_left_distance = read_distance_ultrasound(LEFT); */
    /*   ultrasound_right_distance = read_distance_ultrasound(RIGHT); */

    /*   if (!ultrasound_right_distance) { */
    /*     ultrasound_right_distance = ultrasound_right_distance_last; */
    /*   } else { */
    /*     ultrasound_right_distance_last = ultrasound_right_distance; */
    /*   } */
    /*   if (!ultrasound_left_distance) { */
    /*     ultrasound_left_distance = ultrasound_left_distance_last; */
    /*   } else { */
    /*     ultrasound_left_distance_last = ultrasound_left_distance; */
    /*   } */
    /*   /\* Serial.print("US_L: "); Serial.println(ultrasound_left_distance); *\/ */
    /*   /\* Serial.print("US_R: "); Serial.println(ultrasound_right_distance); *\/ */
    /*   /\* vibration motor actuation *\/ */
    /*   if (ultrasound_left_distance < 200) { */
    /*     buzz_motor(SFL, intensity_map(SFL, ultrasound_left_distance)); */
    /*   } */

    /*   if (ultrasound_right_distance < 200) { */
    /*     buzz_motor(SFR, intensity_map(SFR, ultrasound_right_distance)); */
    /*   } */
    /*   debug_count++; */
    /*   break; */
    /* } */
    /* case (VL_NLR): { */
    /*   vl53_left_distance = read_distance_vl53(LEFT); */
    /*   vl53_right_distance = read_distance_vl53(RIGHT); */

    /*   if (vl53_right_distance == 819) { */
    /*     vl53_right_distance = vl53_right_distance_last; */
    /*   } else { */
    /*     vl53_right_distance_last = vl53_right_distance; */
    /*   } */
    /*   if (vl53_left_distance == 819) { */
    /*     vl53_left_distance = vl53_left_distance_last; */
    /*   } else { */
    /*     vl53_left_distance_last = vl53_left_distance; */
    /*   } */

    /*   /\* vl53 motor actuation *\/ */
    /*   /\* Serial.print("VL53_L: "); Serial.println(vl53_left_distance); *\/ */
    /*   /\* Serial.print("VL53_R: "); Serial.println(vl53_right_distance); *\/ */
    /*   if (vl53_left_distance < 200) { */
    /*     buzz_motor(SNL, intensity_map(SNL, vl53_left_distance)); */
    /*   } */

    /*   if (vl53_right_distance < 200) { */
    /*     buzz_motor(SNR, intensity_map(SNR, vl53_right_distance)); */
    /*   } */
    /*   debug_count++; */
    /*   break; */
    /* } */
    /* case (USSharpLidar_C): { */
    /*   ultrasound_centre_distance = read_distance_ultrasound(CENTRE); */

    /*   sharp_left_distance = read_distance_sharp(LEFT); */
    /*   sharp_right_distance = read_distance_sharp(RIGHT); */

    /*   if (!ultrasound_centre_distance) { */
    /*     ultrasound_centre_distance = ultrasound_centre_distance_last; */
    /*   } else { */
    /*     ultrasound_centre_distance_last = ultrasound_centre_distance; */
    /*   } */

    /*   /\* Serial.print("US_C: "); Serial.println(ultrasound_centre_distance); *\/ */
    /*   /\* Serial.print("Sharp_L: "); Serial.println(sharp_left_distance); *\/ */
    /*   /\* Serial.print("Sharp_R: "); Serial.println(sharp_right_distance); *\/ */

    /*   if (ultrasound_centre_distance > 100) { */
    /*     /\* Serial.print("SL/SR: "); Serial.println((sharp_right_distance + sharp_left_distance)/2.0); *\/ */
    /*     buzz_motor(SC, intensity_map(SC, (sharp_right_distance + sharp_left_distance)/2.0)); */
    /*   } else { */
    /*     /\* Serial.print("US: "); Serial.println(ultrasound_centre_distance); *\/ */
    /*     buzz_motor(SC, intensity_map(SC, ultrasound_centre_distance)); */
    /*   } */
    /*   debug_count++; */
    /*   break; */
    /* } */
    /* } */

    /* if (current_millis - previous_millis > 200) { */
    /*   previous_millis = current_millis; */
    /*   switch (ranging_state) { */
    /*   case (US_FLR): */
    /*     /\* Serial.print("US:"); Serial.println(debug_count); *\/ */
    /*     ranging_state = VL_NLR; */
    /*     /\* ranging_state = USSharpLidar_C; *\/ */
    /*     break; */
    /*   case (VL_NLR): */
    /*     /\* Serial.print("VL:"); Serial.println(debug_count); *\/ */
    /*     ranging_state = USSharpLidar_C; */
    /*     break; */
    /*   case (USSharpLidar_C): */
    /*     /\* Serial.print("US + Sharp:"); Serial.println(debug_count); *\/ */
    /*     ranging_state = US_FLR; */
    /*     /\* ranging_state = USSharpLidar_C; *\/ */
    /*     break; */
    /*   } */
    /*   debug_count = 0; */
    /*   buzz_motor(SFL, 0); */
    /*   buzz_motor(SFR, 0); */
    /*   buzz_motor(SC,  0); */
    /*   buzz_motor(SNL, 0); */
    /*   buzz_motor(SNR, 0); */

    /* } */

    /* Serial.print(ultrasound_left_distance); */
    /* Serial.print(" "); */
    /* Serial.print(vl53_left_distance); */
    /* Serial.print(" "); */
    /* Serial.print(sharp_left_distance); */
    /* Serial.print(" "); */
    /* Serial.print(ultrasound_centre_distance); */
    /* Serial.print(" "); */
    /* Serial.print(sharp_right_distance); */
    /* Serial.print(" "); */
    /* Serial.print(vl53_right_distance); */
    /* Serial.print(" "); */
    /* Serial.println(ultrasound_right_distance); */

    /* Serial1.print(ultrasound_left_distance); */
    /* Serial1.print(" "); */
    /* Serial1.print(vl53_left_distance); */
    /* Serial1.print(" "); */
    /* Serial1.print(sharp_left_distance); */
    /* Serial1.print(" "); */
    /* Serial1.print(ultrasound_centre_distance); */
    /* Serial1.print(" "); */
    /* Serial1.print(sharp_right_distance); */
    /* Serial1.print(" "); */
    /* Serial1.print(vl53_right_distance); */
    /* Serial1.print(" "); */
    /* Serial1.println(ultrasound_right_distance); */

  }
  else if (program_state == FALL_DETECTED) {
    /* Serial.println("mode: Fall detected"); */
    // buzz all motors if a fall is detected from the accelerometer
    if (current_millis - previous_vib_millis > 100) {
      previous_vib_millis = current_millis;
      if (vibrate_state == ON) {
        vibrate_state = OFF;
      } else {
        vibrate_state = ON;
      }
      buzz_motor(SFL, vibrate_state ? 255 : 0);
      buzz_motor(SFR, vibrate_state ? 255 : 0);
      buzz_motor(SC,  vibrate_state ? 255 : 0);
      buzz_motor(SNL, vibrate_state ? 255 : 0);
      buzz_motor(SNR, vibrate_state ? 255 : 0);
    }

    // press button to cancel
    if (button_state == PRESSED) {
      program_state = RANGING;
    }
    // call emergency contact if button not pressed
    else if (current_millis - previous_millis > 5000) {
      previous_vib_millis = current_millis;
      /* Serial.println("enter ECI"); */
      Serial1.print("C");
      /* program_state = EMERGENCY_CALL_IMMEDIATE; */
      program_state = RANGING;
    }
  }
  else if (program_state == EMERGENCY_CALL) {
    /* Serial.println("EC"); */
    if (current_millis - previous_vib_millis > 500) {
      previous_vib_millis = current_millis;
      if (vibrate_state == ON) {
        vibrate_state = OFF;
      } else {
        vibrate_state = ON;
      }
      buzz_motor(SFL, vibrate_state ? 255 : 0);
      buzz_motor(SFR, vibrate_state ? 255 : 0);
      buzz_motor(SC,  vibrate_state ? 255 : 0);
      buzz_motor(SNL, vibrate_state ? 255 : 0);
      buzz_motor(SNR, vibrate_state ? 255 : 0);
    }
    /* Serial.println(button_diff); */
    if ((3000 < button_diff) && (button_diff < 5000)) {
      // send to main contact
      Serial1.print("A");
      /* Serial.println("Call A, enter Ranging"); */
      program_state = RANGING;
    } else if (5000 < button_diff) {
      // send to all contacts
      Serial1.print("B");
      /* Serial.println("Call B, enter Ranging"); */
      program_state = RANGING;
    }

    if (current_millis - previous_millis > 6000) {
      /* Serial.println("timeout, enter ranging"); */
      program_state = RANGING;
    }
  }
  else if (program_state == EMERGENCY_CALL_IMMEDIATE) {
    /* Serial.println("ECI"); */
    Serial1.print("A");
    program_state = RANGING;
  }

  // check if fallen
  if (accelerometer_check()) {
    /* Serial.println("Fall detected"); */
    program_state = FALL_DETECTED;
    previous_millis = current_millis;
  }

  /* check if button pressed */
  if ((button_state == NOT_PRESSED) && digitalRead(PIN_BUTTON)) {
    button_state = PRESSED;
    /* Serial.print("A"); */
    /* Serial1.print("A"); */
    button_down_millis = millis();
  } else if ((button_state == PRESSED) && (digitalRead(PIN_BUTTON) == 0)) {
    /* Serial.print("B"); */
    /* Serial1.print("B"); */
    button_state = NOT_PRESSED;
    button_up_millis = millis();
  }
  button_diff = button_up_millis - button_down_millis;

  if ((program_state == RANGING) && (button_state == PRESSED)
      && (current_millis - button_down_millis) > 1000) {
    previous_millis = current_millis;
    vibrate_state = ON;
    /* Serial.println("EC mode"); */
    program_state = EMERGENCY_CALL;
  }

}
