import com.meapsoft.*;
import java.util.LinkedList;

public class Signal {
    private static FFT fft = new FFT(1024);
    private static double[] fft_mag = new double[512];
    private static double[] re = new double[1024];
    private static double[] im = new double[1024];
    private static LinkedList<double[]> spectrogram = new LinkedList<double[]>();

    public static void makeSpectrogram() {
        for (int i = 0; i < 512; i++) {
            spectrogram.addLast(new double[256]);
        }
    }

    public static void getSignal() {
        MpdReader.readFile(re);
    }

    public static void drawSignal() {
        Drawing.drawLineScope(re, im);
    }

    private static void doFFT() {
        fft.fft(re, im);

        for (int i = 0; i < 512; i++) {
            fft_mag[i] = Math.sqrt(re[i] * re[i] + im[i] + im[i]);
        }

        spectrogram.remove();
        normalize(fft_mag);
        spectrogram.addLast(fft_mag.clone());
    }

    private static void normalize(double[] power) {
        double max = power[10];
        for (int i = 10; i < 256; i ++) {
            if (power[i] > max) {
                max = power[i];
            }
        }

        if (0 < max) {
            for (int i = 0; i < 256; i++) {
                //power[i] /= 64;
                power[i] /= max;
            }
        }

    }


    public static void drawFFT() {
        doFFT();
        Drawing.drawLineFFT(fft_mag);
    }

    public static void drawSpectrogram() {
        Drawing.drawSpectrogram(spectrogram);
    }

}
