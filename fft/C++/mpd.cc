#include "mpd.h"

GLFWwindow* window;

int main(void)
{
    FILE *fp = fopen("/tmp/mpd.fifo", "rb");
    std::vector<double> signal(NUM_DOTS, 0);
    std::vector<double> draw_fft(NUM_DOTS/4, 0);
    std::deque< std::vector<double> > spectrogram(NUM_DOTS/2, draw_fft);

    fftw_complex fft_out[NUM_DOTS] = {{0}};
    fftw_plan p = fftw_plan_dft_r2c_1d(NUM_DOTS, signal.data(), fft_out, FFTW_ESTIMATE);

    init();

    while(!glfwWindowShouldClose(window)) {
        glClear(GL_COLOR_BUFFER_BIT);
        get_signal(signal, fp);

        draw_line_scope(signal);
        fftw_execute(p);

        for (int i = 0; i < NUM_DOTS/4; i++) {
            draw_fft[i] = sqrt(fft_out[i][1] * fft_out[i][1] +
                               fft_out[i][0] * fft_out[i][0]);
        }

        draw_line_fft(draw_fft);

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

    printf("Program quit.\n");
    exit(EXIT_SUCCESS);
}
