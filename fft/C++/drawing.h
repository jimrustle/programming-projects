#include <GLFW/glfw3.h>
#include <vector>
#include <deque>

void key(GLFWwindow* window, int key, int scancode, int action, int mods);
void init();
void draw_line_scope(std::vector<double> &data);
void draw_rect(int x, int y);
void draw_line_fft(std::vector<double> &power);
void draw_spectrogram_line(int x, std::vector<double> &spec_line);
void draw_spectrogram(std::deque< std::vector<double> > spectrogram);
void normalize(std::vector<double> &fft);
