#define NUM_DOTS 4096
#define DIV 4

#include <SDL.h>
#include <SDL_opengl.h>
#include <math.h>
extern SDL_Surface *demo_screen;

void window(double* data){
    int i;
    for(i = 0; i < NUM_DOTS; i++){
        /* hann */
        /*data[i] *= 0.5*(1-cos(2*M_PI*i/NUM_DOTS));*/

        /* hamming */
        //data[i] *= 0.54 - 0.46*cos(2*M_PI*i/NUM_DOTS);

        /* blackman-harris */
        data[i] *= 0.35875 - 0.48829*cos(2*M_PI*i/NUM_DOTS)
                   + 0.14128*cos(4*M_PI*i/NUM_DOTS)
                   - 0.01168*cos(6*M_PI*i/NUM_DOTS);
    }
}

void normalize(int* array){
    int i, max;
    max = array[10];
    for (i = 10; i < NUM_DOTS/DIV; i++){
        if (max < array[i]){
            max = array[i];
        }
    }
    if (max != 0){
    for (i = 1; i < NUM_DOTS/DIV; i++){
        array[i] = array[i]*1024/max;
    }}
}

void draw_line_scope(double* data) {
    int i;
    glBegin(GL_LINE_STRIP);
    glColor3f(1, 0, 0);
    for (i = 0; i < NUM_DOTS/DIV; i++){
        glVertex2f(i, data[i]);
    }
    glEnd();
}

void draw_rect(int x, int y){
    y = 512-y;
    glBegin(GL_QUADS); /* Top left, bottom left, bottom right, top right */
    glColor3f(1, 0, 0);

    glVertex2f(x, y);
    glVertex2f(x, 512);
    glVertex2f(x+1, y);
    glVertex2f(x+1, 512);

    glEnd();
}

void draw_line_fft(int* power) {
    int i;
    for (i = 1; i < NUM_DOTS/DIV; i++){
        draw_rect(i, power[i]);
    }
}

void fill_buffer(double* buffer, FILE* fp) {
    int i;
    int val;
    for(i = 0; i < NUM_DOTS; i++) {
        getc(fp); /* Causes noticeable slowdown */
        val = getc(fp);
        buffer[i] = (val < 128) ? 128 - val : 384 - val;
    }
}

void handle_keys(unsigned char key, int* running, int* norm, int* windp){
    *running = !(key == 'q');
    if (key == 'n'){
        *norm = !*norm;
    }
    else if (key == 'w'){
        *windp = !*windp;
    }
}
