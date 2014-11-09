
import java.nio.file.*;
import java.io.*;

public class MpdReader {
    public static FileInputStream file;

    public static void openFile() {
        try {
            file = new FileInputStream("/tmp/mpd.fifo");
        }
        catch (FileNotFoundException e){
            System.out.println("mpd file not found");
        }
    }

    public static double[] readFile(double[] readBuffer) {
        try {
            for (int i = 0; i < 1024; i++) {
                int low = file.read();
                int high = file.read();
                readBuffer[i] = ((high & 0xff) << 8) | (low & 0xff);
                readBuffer[i] = (((readBuffer[i]/256)) + 128) % 256;
            }

        }
        catch (IOException e){
            System.out.println("error reading fifo");
        }
        return readBuffer;
    }

    public static void closeFile() {
        try{
            if (file != null) {
            file.close();
            }
        }
        catch (IOException e){
            System.out.println("error closing file");
        }
    }

}
