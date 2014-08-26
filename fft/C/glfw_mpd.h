#define NUM_DOTS 1024

#include <GL/glfw.h>

#include <math.h>
#include <fftw3.h>

#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

void draw_spec_line(int x, double *array)
{
    int y;
    glBegin(GL_QUAD_STRIP); /* Top left, bottom left, bottom right, top right */

    for (y = 0; y < 256; y++) {
        double colorval = array[y];
        colorval = 1 - colorval;
        glColor3f(colorval, colorval, colorval);
        int y_pos = y + 256;
        glVertex2f(x, y_pos);
        glVertex2f(x+1, y_pos);
    }

    glEnd();
}

void normalize(double *array)
{
    int i;
    double max;
    max = array[1];

    for (i = 1; i < 256; i++) {
        if (max < array[i]) {
            max = array[i];
        }
    }

    if (max > 0) {
        for (i = 1; i < 256; i++) {
            array[i] = array[i]/max;
        }
    }
}

void fill_buffer(double *buffer, FILE* fp)
{
    int16_t buf[NUM_DOTS] = {0};
    fread(buf, sizeof(int16_t), NUM_DOTS, fp);

    ssize_t i;

    for (i = 0; i < NUM_DOTS; i++) {
        buffer[i] = (buf[i] >> 8) + 128.0;
    }
}

void draw_line_scope(double* data)
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

void draw_line_fft(double* power)
{
    int i;
    int val;
    for (i = 1; i < NUM_DOTS/2; i++) {
        val = power[i]/20.0;
        val = fmin(val, 256);
        draw_rect(2*i, val);
    }
}
