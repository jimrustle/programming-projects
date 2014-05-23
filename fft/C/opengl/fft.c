#include <time.h>
#include <stdlib.h>
#include <fftw3.h>
#include "fft.h"

SDL_Surface *demo_screen;

int main(void) {
    SDL_Event ev;

    int i;
    double fft_in[NUM_DOTS] = {0};
    fftw_complex fft_out[NUM_DOTS] = {{0}};

    fftw_plan p = fftw_plan_dft_r2c_1d(NUM_DOTS, fft_in, fft_out, FFTW_MEASURE);

    /*int draw_input[NUM_DOTS] = {0};*/
    int draw_output[NUM_DOTS/DIV] = {0};

    FILE *fp = fopen("/tmp/mpd.fifo", "rb");

    /* Set up SDL */

    if(SDL_Init(SDL_INIT_VIDEO) != 0)
        fprintf(stderr, "Could not initialize SDL: %s\n", SDL_GetError());

    demo_screen = SDL_SetVideoMode(NUM_DOTS/DIV, 512, 0, SDL_OPENGL | \
            SDL_HWSURFACE | SDL_DOUBLEBUF);
    if(!demo_screen)
        fprintf(stderr, "Could not set video mode: %s\n", SDL_GetError());

    SDL_WM_SetCaption("MPD Visalizer - Sanic Fast OpenGL Edition", NULL);

    /* Set up OpenGL */
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
    glClearColor( 1.f, 1.f, 1.f, 1.f ); /* White clear/background colour */
    glOrtho(0, 1024, 512, 0, 0, 1);

    GLenum error = glGetError();
    if( error != GL_NO_ERROR ) {
        printf("Error initializing OpenGL!");
    }

    /* Main Loop */
    int running = 1;
    int norm = 0;
    int windp = 0;
    while(running) {
        /* Catch events */
        while(SDL_PollEvent(&ev)) {
            if(ev.type == SDL_QUIT){
                running = 0;
            }
            else if(ev.type == SDL_KEYDOWN ) {
                /* Quit if 'q', change normalise if 'n' */
                unsigned char key = ev.key.keysym.sym;
                handle_keys(key, &running, &norm, &windp);
            }
        }
        /* Blank the screen to white background */
        glClear(GL_COLOR_BUFFER_BIT);

        /* Fill the input buffer and draw it to the screen */
        fill_buffer(fft_in, fp);

        if (windp){
            window(fft_in);
        }

        /*draw_line_scope(fft_in);*/

        fftw_execute(p);
        for(i = 0; i < NUM_DOTS/DIV; i++) {
            draw_output[i] = sqrt(fft_out[i][1] * fft_out[i][1] +
                    fft_out[i][0] * fft_out[i][0]);
        }

        if (norm){
            normalize(draw_output);
        }

        /* Draw, then update the screen */
        draw_line_fft(draw_output);

        SDL_GL_SwapBuffers();
    }

    SDL_Quit();
    fclose(fp);
    printf("Program quit.\n");
    return 0;
}
