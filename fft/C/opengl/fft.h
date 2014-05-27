#define NUM_DOTS 4096
#define DIV 4

#include <SDL.h>
#include <SDL_opengl.h>
#include <math.h>

#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>
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

void fill_buffer(double* buffer, int fp) {
    int16_t buf[2*NUM_DOTS] = {0};

    read(fp, buf, sizeof(buf));

    ssize_t i;
    for (i = 0; i < 2*NUM_DOTS; i+=2){
        buffer[i] = buf[i];
    }

    //unsigned char tmp[NUM_DOTS*2];
    //fread(tmp, sizeof(unsigned char), 2*NUM_DOTS, fp);

    //int i;
    //for (i = 1; i < NUM_DOTS; i++){
        //double value = tmp[2*i];
        //buffer[i] = (value < 128) ? 128 - value : 384 - value;
        ////buffer[i] = (double) tmp[2*i];
    //}
}

void handle_keys(unsigned char key, int* running, int* norm, int* windp,
        int* m, int* d, int* val){
    *running = !(key == 'q');
    if (key == 'n'){
        *norm = !*norm;
    }
    else if (key == 'w'){
        *windp = !*windp;
    }
    else if (key == 'm'){
        *m = !*m;
    }
    else if (key == 'd'){
        *d = !*d;
    }
    else if (key == 'k'){
        *val += 1;
    }
    else if (key == 'j'){
        if (*val > 2){
        *val -= 1;
        }
    }
}
