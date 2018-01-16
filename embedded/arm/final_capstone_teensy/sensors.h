
typedef enum direction_t { LEFT, RIGHT, CENTRE } direction_t;
bool accelerometer_check(void);
int read_distance_sharp(direction_t direction);
int read_distance_vl53(direction_t direction);
int read_distance_ultrasound(direction_t direction);
void remap_vl53_sensors(void);

