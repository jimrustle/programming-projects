#include "gps.h"

// David Johnson-Davies @ technoblogy.com

/* http://aprs.gids.nl/nmea */
/* $GPRMC*/
/* Recommended minimum specific GPS/Transit data*/

/* eg1. $GPRMC,081836,A,3751.65,S,14507.36,E,000.0,360.0,130998,011.3,E*62*/
/* eg2. $GPRMC,225446,A,4916.45,N,12311.12,W,000.5,054.7,191194,020.3,E*68*/


            /*225446       Time of fix 22:54:46 UTC*/
            /*A            Navigation receiver warning A = OK, V = warning*/
            /*4916.45,N    Latitude 49 deg. 16.45 min North*/
            /*12311.12,W   Longitude 123 deg. 11.12 min West*/
            /*000.5        Speed over ground, Knots*/
            /*054.7        Course Made Good, True*/
            /*191194       Date of fix  19 November 1994*/
            /*020.3,E      Magnetic variation 20.3 deg East*/
            /**68          mandatory checksum*/


/* eg3. $GPRMC,220516,A,5133.82,N,00042.24,W,173.8,231.8,130694,004.2,W*70*/
               /*1    2    3    4    5     6    7    8      9     10  11 12*/


       /*1   220516     Time Stamp*/
       /*2   A          validity - A-ok, V-invalid*/
       /*3   5133.82    current Latitude*/
       /*4   N          North/South*/
       /*5   00042.24   current Longitude*/
       /*6   W          East/West*/
       /*7   173.8      Speed in knots*/
       /*8   231.8      True course*/
       /*9   130694     Date Stamp*/
       /*10  004.2      Variation*/
       /*11  W          East/West*/
       /*12  *70        checksum*/


/* eg4. $GPRMC,hhmmss.ss,A,llll.ll,a,yyyyy.yy,a,x.x,x.x,ddmmyy,x.x,a*hh*/
/* 1    = UTC of position fix*/
/* 2    = Data status (V=navigation receiver warning)*/
/* 3    = Latitude of fix*/
/* 4    = N or S*/
/* 5    = Longitude of fix*/
/* 6    = E or W*/
/* 7    = Speed over ground in knots*/
/* 8    = Track made good in degrees True*/
/* 9    = UT date*/
/* 10   = Magnetic variation degrees (Easterly var. subtracts from true course)*/
/* 11   = E or W*/
/* 12   = Checksum*/

char fmt[]="$GPRMC,dddtdd.ds,A,eeae.eeee,l,eeeae.eeee,o,jdk,c,dddy";

int state = 0;
unsigned int t;
long ltmp;

void parse_GPS (char c) {
  if (c == '$') { state = 0; t = 0; ltmp = 0; }
  char mode = fmt[state++];
  // If received character matches format string, return
  if (mode == c) return;
  unsigned int d = (unsigned int) c - '0';
  // Ignore extra digits of precision
  if (mode == ',') state--; 
  // d=decimal digit; j=decimal digits before decimal point
  else if (mode == 'd') t = t*10 + d;
  else if (mode == 'j') { if (c != '.') { t = t*10 + d; state--; } }
  // e=long decimal digit
  else if (mode == 'e') ltmp = (ltmp<<3) + (ltmp<<1) + d; // ltmp = ltmp*10 + d;
  // a=angular measure
  else if (mode == 'a') ltmp = (ltmp<<2) + (ltmp<<1) + d; // ltmp = ltmp*6 + d;
  // t=Time - hhmm
  else if (mode == 't') { gps_time = t*10 + d; t = 0; }
  // s=Centisecs
  else if (mode == 's') { gps_csecs = t*10 + d; t = 0; }
  // l=Latitude - in minutes*1000
  else if (mode == 'l') { if (c == 'N') gps_lat = ltmp; else gps_lat = -ltmp; ltmp = 0; }
  // o=Longitude - in minutes*1000
  else if (mode == 'o') { if (c == 'E') gps_long = ltmp; else gps_long = -ltmp; ltmp = 0; }
   // k=Speed - in knots*100
  else if (mode == 'k') { gps_knots = t*10 + d; t = 0; }
  // c=Course (Track) - in degrees*100. Allow for empty field.
  else if (mode == 'c') {
    if (c == ',') { gps_course = t; t = 0; state++; }
    else if (c == '.') state--;
    else { t = t*10 + d; state--; }
  }
  // y=Date - ddmm
  else if (mode == 'y') { gps_date = t*10 + d ; gps_fix = 1; state = 0; }
  else state = 0;
}
