/*
 * SDL FFT Spectrum Audio Display
 * 30 March 2014
 */

#define NUM_DOTS 1024

#include <time.h>
#include <SDL.h>
#include <stdlib.h>
#include <kiss_fft.h>
#include <kiss_fftr.h>
#include <math.h>

SDL_Surface *demo_screen;

void draw_line(int xi, int yi, int xf, int yf) {
    /* Draws a line from point to point using Bresenham's line algorithm */
    int rank;
    Uint32 *pixel;
    /*SDL_LockSurface(demo_screen);*/
    rank = demo_screen->pitch / sizeof(Uint32);
    pixel = (Uint32 *) demo_screen->pixels;
    int dx = abs(xf - xi);
    int dy = abs(yf - yi);
    int sx = (xi < xf) ? 1 : -1;
    int sy = (yi < yf) ? 1 : -1;

    int err = dx - dy;

    while(1) {
        pixel[xi + yi * rank] = SDL_MapRGB(demo_screen->format, 255, 0, 0);
        if ((xi == xf) && (yi == yf)) {
            break;
        }
        int e2 = 2 * err;
        if (e2 > -dy) {
            err -= dy;
            xi += sx;
        }
        if (e2 < dx) {
            err += dx;
            yi += sy;
        }
    }
    /*SDL_UnlockSurface(demo_screen);*/
}

void draw_line_scope(int* data) {
    /* Calls Bresenham's line algorithm to draw an array of points */
    int i;
    for(i = 0; i < NUM_DOTS - 1; i++) {
        draw_line(i, data[i], i + 1, data[i+1]);
    }
}

void draw_line_fft(int* power) {
    int i;
    SDL_Rect rect;
    SDL_LockSurface(demo_screen);
    rect.w = 2;
    for(i = 1; i < NUM_DOTS / 2; i++) {
        rect.x = i * 2;
        rect.y = 512 - power[i];
        rect.y = rect.y < 256 ? 256 : rect.y;
        rect.h = 512 - rect.y;
        SDL_FillRect(demo_screen, &rect, SDL_MapRGBA(demo_screen->format, \
                    255, 0, 0, 255));
    }
    SDL_UnlockSurface(demo_screen);
}

void fill_buffer(int* buffer, FILE* fp) {
    int i;
    int val;
    for(i = 0; i <= NUM_DOTS; i++) {
        getc(fp); /* No idea why, but this improves the input data */
        val = getc(fp);
        buffer[i] = (val < 128) ? 128 - val : 384 - val;
    }
}

int main(void) {
    SDL_Event ev;
    int i;

    kiss_fftr_cfg cfg = kiss_fftr_alloc(NUM_DOTS, 0, NULL, NULL);
    kiss_fft_scalar fft_input[1024] = {0};
    kiss_fft_cpx fft_output[512] = {{0,0}};

    int draw_input[1024] = {0};
    int draw_output[512] = {0};

    FILE *fp = fopen("/tmp/mpd.fifo", "rb");

    if(SDL_Init(SDL_INIT_VIDEO) != 0)
        fprintf(stderr, "Could not initialize SDL: %s\n", SDL_GetError());

    demo_screen = SDL_SetVideoMode(NUM_DOTS, 512, 0, SDL_HWSURFACE | SDL_DOUBLEBUF);
    if(!demo_screen)
        fprintf(stderr, "Could not set video mode: %s\n", SDL_GetError());

    int running = 1;
    while(running) {
        /* Catch exit events */
        while(SDL_PollEvent(&ev)) {
            if(ev.type == SDL_QUIT)
                running = 0;
        }

        /* Blank the screen to black background */
        SDL_FillRect(demo_screen, NULL, SDL_MapRGB(demo_screen->format, \
                    255, 255, 255));

        /* Fill the input buffer and draw it to the screen */
        fill_buffer(draw_input, fp);
        draw_line_scope(draw_input);

        /* Convert the input buffer to one suitable for the real fft
         * by casting integers to floats */
        for (i = 0; i < NUM_DOTS; i++) {
            fft_input[i] = draw_input[i];
        }

        /* Calculate the fft and find the magnitudes for the first 512
         * frequency bins, divide by 5 to pretty up the display */
        kiss_fftr(cfg, fft_input, fft_output);
        for(i = 0; i <= NUM_DOTS / 2; i++) {
            draw_output[i] = sqrt(fft_output[i].i * fft_output[i].i + \
                    fft_output[i].r * fft_output[i].r)/20;
        }

        /* Draw, then update the screen */
        draw_line_fft(draw_output);

        SDL_Flip(demo_screen);
    }

    SDL_Quit();
    fclose(fp);
    printf("Program quit.\n");
    return 0;
}
