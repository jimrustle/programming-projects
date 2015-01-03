#include "constants.h"
#include "drawing.h"
#include <math.h>
#include <fftw3.h>
#include <stdio.h>

int main()
{
    FILE* fp = fopen("/tmp/mpd.fifo", "rb");

    if (fp == NULL) {
        printf("ayy, /tmp/mpd.fifo not found -- is mpd running?\n");
        return 1;
    }

    unsigned int i;
    double signal[NUM_DOTS] = {0};

    fftw_complex fft_out[NUM_DOTS] = {{0}};

    fftw_plan p = fftw_plan_dft_r2c_1d(NUM_DOTS, signal, fft_out, FFTW_MEASURE);

    /* circular buffer -- shift the pointers instead of using
       memcpy or memmove */

    unsigned int start = 0;
    /* 256 = number of pixels to store vertically
     * 512= number of lines of pixels to store horizontally */
    double outputs[256*512] = {0};

    GLFWwindow* window;
    window = init();

    while(!glfwWindowShouldClose(window)) {

        glClear(GL_COLOR_BUFFER_BIT);

        /* Fill the input buffer and draw it to the screen */
        fill_buffer(signal, fp);

        /* optional windowing */
        /*hann_window(signal);*/

        draw_line_scope(signal);

        /* execute FFT on the input buffer */
        fftw_execute(p);

        /* write the magnitudes of the FFT into the oldest location*/
        /* of the circular buffer */
        for (i = 0; i < 256; i++) {
            outputs[start*256 + i] = sqrt(fft_out[i][1] * fft_out[i][1] +
                                          fft_out[i][0] * fft_out[i][0]);
        }

        /* draw it without normalization */
        draw_line_fft(outputs+start*256);

        /* normalize (in-place) and store it within the circular buffer */
        normalize(outputs+start*256);

        /* then shift the index of the oldest spectrogram line in the
         * array by one */
        start++;
        start %= 512;

        /* starting from that "start" position, plot the next 256 spectrogram
         * lines of data */
        unsigned int x;
        unsigned int address;
        for (x = 0; x < 512; x++) {
            address = start + x;
            address %= 512;
            draw_spec_line(x, outputs + (address * 256));
        }

        /* update screen */
        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    fclose(fp);

    glfwDestroyWindow(window);
    glfwTerminate();

    printf("Program quit.\n");

    return 0;
}

