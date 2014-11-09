import static com.badlogic.jglfw.Glfw.*;
import static com.badlogic.jglfw.gl.GL.*;

public class MpdDisplay {
    private static final int NUM_DOTS = 1024;


    public static void main(String[] argv) {

        long window = Drawing.init();
        MpdReader.openFile();

        Signal.makeSpectrogram();

        while (!glfwWindowShouldClose(window)) {
            glClear(GL_COLOR_BUFFER_BIT);

            Signal.getSignal();

            Signal.drawSignal();

            Signal.drawFFT();

            Signal.drawSpectrogram();

            glfwSwapBuffers(window);
            glfwPollEvents();
        }

        MpdReader.closeFile();

        glfwDestroyWindow(window);
        glfwTerminate();

        System.out.println("Program quit.");
    }
}
