#include "constants.h"
#include <GLFW/glfw3.h>

#include <vector>
#include <cmath>
#include <deque>
#include <algorithm>

extern GLFWwindow* window;

void key(GLFWwindow* window, int key, int scancode, int action, int mods)
{
    if (key == 'Q') {
        glfwSetWindowShouldClose(window, GL_TRUE);
    }
}

void init()
{
    glfwInit();
    glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);
    window = glfwCreateWindow(1024, 512, "MPD Visualizer", NULL, NULL);

    glfwSetKeyCallback(window, key);
    glfwMakeContextCurrent(window);

    //glfwSwapInterval(1);
    glLoadIdentity();
    glDisable(GL_ALPHA_TEST);
    glDisable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_DITHER);
    glDisable(GL_FOG);
    glDisable(GL_LIGHTING);
    glDisable(GL_LOGIC_OP);
    glDisable(GL_STENCIL_TEST);
    glDisable(GL_TEXTURE_1D);
    glDisable(GL_TEXTURE_2D);

    /* Background to black */
    glClearColor(1.f, 1.f, 1.f, 0.f);
    glOrtho(0, 1024, 0, 512, 0, 1);
}

void draw_line_scope(std::vector<double> &data)
{
    int i;
    glBegin(GL_LINE_STRIP);
    glColor3f(1, 0, 0);
    for (i = 0; i < NUM_DOTS; i++) {
        glVertex2f(i, data[i]);
    }
    glEnd();
}

void draw_rect(int x, int y)
{
    glBegin(GL_QUADS); /* Top left, bottom left, bottom right, top right */
    glColor3f(1, 0, 0);
    y += 256;

    x += 512;
    glVertex2f(x, y);
    glVertex2f(x, 256);
    glVertex2f(x+1, 256);
    glVertex2f(x+1, y);

    glEnd();
}

void draw_line_fft(std::vector<double> &power)
{
    int i;
    int val;

    for (i = 1; i < NUM_DOTS/4; i++) {
        val = power[i]/20.0;
        val = fmin(val, 256);
        draw_rect(2*i, val);
    }
}

void draw_spectrogram_line(int x, std::vector<double> &spec_line)
{
    int y;
    glBegin(GL_QUAD_STRIP); /* Top left, bottom left, bottom right, top right */

    for (y = 0; y < 256; y++) {
        double colorval = spec_line[y];
        colorval = 1 - colorval;
        glColor3f(colorval, colorval, colorval);
        int y_pos = y + 256;
        glVertex2f(x, y_pos);
        glVertex2f(x+1, y_pos);
    }

    glEnd();
}

void draw_spectrogram(std::deque<std::vector<double>> spectrogram)
{
    for (auto spec_line = spectrogram.begin(); spec_line != spectrogram.end(); spec_line++) {
        int x_pos = spec_line - spectrogram.begin();
        draw_spectrogram_line(x_pos, *spec_line);
    }
}

void normalize(std::vector<double> &fft)
{
    double max = *std::max_element(fft.begin()+3, fft.end());
    if (0 < max) {
        std::transform(fft.begin(), fft.end(), fft.begin(), [max](double x) {
            return x/max;
        });
    }
}
