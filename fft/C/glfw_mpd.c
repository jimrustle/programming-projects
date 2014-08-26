#include "glfw_mpd.h"

static int running = 1;

void GLFWCALL key(int k, int action)
{
    if (action != GLFW_PRESS) {
        return;
    }
    if (k == 'Q') {
        running = 0;
    }
}

static void init()
{
    /* lol no error handling */
    glfwInit();
    glfwOpenWindowHint(GLFW_WINDOW_NO_RESIZE, GL_TRUE);
    glfwOpenWindow(1024, 512, 0, 0, 0, 0, 16, 0, GLFW_WINDOW);
    glfwSetWindowTitle("MPD Visualizer");

    glfwEnable(GLFW_KEY_REPEAT);
    glfwSetKeyCallback(key);

    glfwSwapInterval(1);
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

int main()
{
    int i;
    double signal[NUM_DOTS] = {0};
    fftw_complex fft_out[NUM_DOTS] = {{0}};

    fftw_plan p = fftw_plan_dft_r2c_1d(NUM_DOTS, signal, fft_out, FFTW_MEASURE);

    /* circular buffer -- shift the pointers instead of using
       memcpy or memmove */

    int start = 0;
    /* 256 = number of pixels to store vertically
     * 512= number of lines to store horizontally */
    double *outputs = malloc(256*512*sizeof(double));

    FILE* fp = fopen("/tmp/mpd.fifo", "rb");

    init();

    while (running) {
        glClear(GL_COLOR_BUFFER_BIT);

        /* Fill the input buffer and draw it to the screen */
        fill_buffer(signal, fp);

        draw_line_scope(signal);

        /* execute FFT on the input buffer */
        fftw_execute(p);

        /* write the magnitudes of the FFT into the oldest location
         * of the circular buffer */
        for (i = 0; i < 256; i++) {
            (outputs+start*256)[i] = sqrt(fft_out[i][1] * fft_out[i][1] +
                                          fft_out[i][0] * fft_out[i][0]);
        }

        /* draw it without normalization */
        draw_line_fft(outputs+start*256);

        /* normalize and store within the circular buffer */
        normalize(outputs+start*256);

        /* then shift the index of the oldest array by one */
        start++;
        start %= 512;

        int x;
        for (x = 0; x < 512; x++) {
            /*int address = start + 256 * x;*/
            int address = (start+x)*256;
            address &= 256*511;
            draw_spec_line(x, &outputs[address]);
        }

        /* update screen */
        glfwSwapBuffers();

        if (!glfwGetWindowParam(GLFW_OPENED)) {
            running = 0;
        }
    }

    free(outputs);
    fclose(fp);

    printf("Program quit.\n");

    glfwTerminate();
    exit(EXIT_SUCCESS);
}
