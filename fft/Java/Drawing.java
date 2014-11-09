import static com.badlogic.jglfw.Glfw.*;
import static com.badlogic.jglfw.gl.GL.*;
import com.badlogic.jglfw.GlfwCallbackAdapter;

import java.util.LinkedList;

public class Drawing {
    public static long init() {
        glfwInit();
        long window = glfwCreateWindow(1024, 512, "MPD Display", 0, 0);

        glfwSetCallback(new GlfwCallbackAdapter() {
            @Override
            public void key (long window, int key, int action) {
                if (key == 'Q') {
                    glfwSetWindowShouldClose(window, 1);
                }
            }
        });

        glfwMakeContextCurrent(window);
        glOrtho(0.0f, 1024.0f, 0.0f, 512.0f, 0.0f, 1.0f);
        glClearColor(1, 1, 1, 1);

        return window;
    }

    public static void drawLineScope(double[] signal, double[] im) {
        glColor3f(1, 0, 0);
        glBegin(GL_LINE_STRIP);
        for (int i = 0; i < 1024; i++) {
            glVertex2f(i, (int) signal[i]);
            im[i] = 0;
        }
        glEnd();
    }


    private static void drawRect(int x, int y) {
        glBegin(GL_QUADS);
        y = y + 256;
        x = x + 512;
        glVertex2f(x, y);
        glVertex2f(x, 256);
        glVertex2f(x+1, 256);
        glVertex2f(x+1, y);

        glEnd();
    }

    public static void drawLineFFT(double[] power) {
        for (int i = 1; i < 256; i++) {
            drawRect(2*i-1, (int) power[i]);
        }
    }

    private static void drawSpectrogramLine(int x, double[] spec_line) {
        glBegin(GL_QUAD_STRIP); // Top left, bottom left, bottom right, top right

        for (int y = 0; y < 256; y++) {
            float colorval = (float) spec_line[y];
            colorval = 1 - colorval;
            glColor3f(colorval, colorval, colorval);
            int y_pos = y + 256;
            glVertex2f(x, y_pos);
            glVertex2f(x+1, y_pos);
        }

        glEnd();
    }

    public static void drawSpectrogram(LinkedList<double[]> ll) {
        for (int i = 0; i < 512; i++) {
            drawSpectrogramLine(i, ll.get(i));
        }
    }
}
