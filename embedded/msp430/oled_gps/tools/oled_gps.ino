#include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>
#include <TinyGPS.h>
#include <SoftwareSerial.h>

#define UTC_TIME_OFFSET (-5)
#define OLED_RESET 4
#define GPS_RX 11
#define GPS_TX 10
Adafruit_SSD1306 display(OLED_RESET);
TinyGPS gps;
//SoftwareSerial nss(GPS_TX, GPS_RX);
//Serial nss(0, 1);

#if (SSD1306_LCDHEIGHT != 64)
#error("Height incorrect, please fix Adafruit_SSD1306.h!");
#endif

typedef struct time_24h {
  uint8_t hour;
  uint8_t minute;
  uint8_t second;
};

void setup()   {
  Serial.begin(9600);
  //  while (!Serial) {
  //    ; // wait for serial port to connect. Needed for native USB port only
  //  }
  //
  //  Serial.print("hi");

  //nss.begin(9600);

  display.begin(SSD1306_SWITCHCAPVCC, 0x3C);  // initialize with the I2C addr 0x3C (for the 128x64)
  display.clearDisplay();
  display.setTextColor(WHITE);
}

void loop() {
  bool newData = false;
  unsigned long fix_age;
  int year;
  uint8_t hour, minute, second, tmp;

  for (unsigned long start = millis(); millis() - start < 1000;) {
    while (Serial.available()) {
      int c = Serial.read();
      if (gps.encode(c)) newData = true;
    }
  }

  display.clearDisplay();
  display.setCursor(0, 0);

  if (newData) {
    display.setTextSize(1);
    display.print(fix_age); display.print(" ");
    display.print(gps.f_altitude()); display.print(" m ");
    display.print(gps.f_course()); display.print((char)247);

    display.setCursor(0, 16);
    display.setTextSize(3);
    gps.crack_datetime(&year, &tmp, &tmp, &hour, &minute, &second, &tmp, &fix_age);
    display.print(gps.f_speed_kmph());

    display.setCursor(75, 30);
    display.setTextSize(1);
    display.println("km/h");

    display.setTextSize(2);
    display.setCursor(0, 48);
    display.print(hour + UTC_TIME_OFFSET);
    display.print(":"); display.print(minute);
    display.print(":"); display.print(second);
  } else {
    display.print("No Data");
  }
  display.display();
}

