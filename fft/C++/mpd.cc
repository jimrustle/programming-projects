#include "constants.h"
#include "drawing.h"
#include "reading.h"

#include <fftw3.h>

#include <cmath>
#include <iostream>

GLFWwindow* window;

int main(void)
{
    FILE *fp = fopen("/tmp/mpd.fifo", "rb");

    if (fp == NULL) {
        std::cout << "ayy, /tmp/mpd.fifo not found -- is mpd running?"
            << std::endl;
        return 1;
    }

    std::vector<double> signal(NUM_DOTS, 0);
    std::vector<double> draw_fft(NUM_DOTS/4, 0);
    std::deque< std::vector<double> > spectrogram(NUM_DOTS/2, draw_fft);

    fftw_complex fft_out[NUM_DOTS] = {{0}};
    fftw_plan p = fftw_plan_dft_r2c_1d(NUM_DOTS, signal.data(), fft_out,
                                       FFTW_MEASURE);

    init();

    while (!glfwWindowShouldClose(window)) {
        glClear(GL_COLOR_BUFFER_BIT);

        // get mpd audio signal
        get_signal(signal, fp);

        draw_line_scope(signal);

        fftw_execute(p);

        // clear the fft vector, then store new fft data to it
        draw_fft.clear();

        for (int i = 0; i < NUM_DOTS/4; i++) {
            draw_fft.push_back(sqrt(fft_out[i][1] * fft_out[i][1] +
                               fft_out[i][0] * fft_out[i][0]));
        }

        draw_line_fft(draw_fft);

        // remove the frontmost spectrogram line from the deque, then
        // push the newest one onto the end
        spectrogram.pop_front();
        normalize(draw_fft);
        spectrogram.push_back(draw_fft);

        draw_spectrogram(spectrogram);

        glfwPollEvents();
        glfwSwapBuffers(window);

    }

    glfwDestroyWindow(window);
    glfwTerminate();
    fclose(fp);

    std::cout << "Program quit." << std::endl;
    return 0;
}
