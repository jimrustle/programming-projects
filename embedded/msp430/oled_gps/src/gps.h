
#include <stdbool.h>

// GPS variables
volatile unsigned int gps_time, gps_csecs, gps_knots, gps_course, gps_date;
volatile long gps_lat, gps_long;

void parse_GPS(char c);
