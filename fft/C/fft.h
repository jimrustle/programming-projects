#define NUM_DOTS 8192
#define DIV 8

#include <SDL.h>
#include <SDL_opengl.h>

#include <math.h>
#include <fftw3.h>

#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>

extern SDL_Surface *demo_screen;

void filter(double *data)
{
    int i;
    for (i = 1; i < NUM_DOTS/DIV-1; i++) {
        data[i] = (data[i-1] + data[i] + data[i+1]) / 3.0;
    }
}

void window(double *data, int s)
{
    int i;

    /* hann */
    if (s == 0) {
        for (i = 0; i < NUM_DOTS/DIV; i++) {
            data[i] *= 0.5 * (1 - cos(2 * M_PI * i / (NUM_DOTS/DIV)));
        }
    }

    /* exp poisson */
    else if (s == 1) {
        for (i = 0; i < NUM_DOTS/DIV; i++) {
            /* hamming */
            /* data[i] *= 0.54 - 0.46*cos(2*M_PI*i/(NUM_DOTS/DIV)); */
            data[i] *= exp(-abs(i - 511.5)*1/(4449.28/30.0));
        }
    }

    /* blackman-harris */
    else if (s == 2) {
        for (i = 0; i < NUM_DOTS/DIV; i++) {
            data[i] *= 0.35875 - 0.48829*cos(2*M_PI*i/(NUM_DOTS/DIV))
                       + 0.14128*cos(4*M_PI*i/(NUM_DOTS/DIV))
                       - 0.01168*cos(6*M_PI*i/(NUM_DOTS/DIV));
        }
    }
}

void normalize(int *array)
{
    int i, max;
    max = array[1];

    for (i = 1; i < NUM_DOTS / DIV; i++) {
        if (max < array[i]) {
            max = array[i];
        }
    }

    if (0 < max) {
        for (i = 1; i < NUM_DOTS / DIV; i++) {
            array[i] = array[i] * 512 / max;
        }
    }
}

void draw_line_scope(double *data)
{
    int i;
    glBegin(GL_LINE_STRIP);
    glColor3f(1, 0, 0);

    for (i = 0; i < NUM_DOTS / DIV; i++) {
        glVertex2f(i, data[i]);
    }

    glEnd();
}

void draw_rect(int x, int y)
{
    y += 256;
    glBegin(GL_QUADS);  /* Top left, bottom left, bottom right, top right */
    glColor3f(1, 0, 0);

    glVertex2f(x, y);
    glVertex2f(x, 256);
    glVertex2f(x + 1, 256);
    glVertex2f(x + 1, y);

    glEnd();
}

void draw_line_fft(int *power)
{
    int i;

    for (i = 0; i < NUM_DOTS / DIV; i++) {
        draw_rect(i, power[i]);
    }
}

void fill_buffer(double *buffer, int fp)
{
    int16_t buf[NUM_DOTS] = {0};

    read(fp, buf, sizeof(buf));

    ssize_t i;

    for (i = 0; i < NUM_DOTS; i++) {
        buffer[i] = buf[i] / 256.0 + 128;
    }
}

void handle_keys(unsigned char key, int* running, int* windp,
                 int* k, int* d, int* val, int* f)
{
    *running = !(key == 'q');
    if (key == 'w') {
        *windp = !*windp;
    } else if (key == 'd') {
        *d = !*d;
    } else if (key == 'c') {
        *k = ((*k+1) % 3);
    } else if (key == 'k') {
        *val += 1;
    } else if (key == 'j') {
        if (*val > 2) {
            *val -= 1;
        }
    } else if (key == 'f') {
        *f = !*f;
    }
}
