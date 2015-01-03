#include <GLFW/glfw3.h>
#include <stdio.h>

void key(GLFWwindow* window, int key, int scancode, int action, int mods);
GLFWwindow* init();
void draw_spec_line(int x, double *array);
void normalize(double *array);
void fill_buffer(double *buffer, FILE* fp);
void draw_line_scope(double* data);
void draw_rect(int x, int y);
void draw_line_fft(double* power);
void hann_window(double *signal);

