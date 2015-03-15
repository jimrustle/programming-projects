import java.math.BigInteger;

public class BigTest {
    static int MAXNUMINTS = 100;
    static int MAXRUN = 25;

    public static void main(String[] args) {
        HugeInteger hugeA, hugeB, hugeC;
        BigInteger bigA, bigB, bigC;
        int tmp;
        long startTime, endTime;
        double runTimeBig = 0.0, runTimeHuge = 0.0;

        //int nArray[] = new int[]{10, 100, 500, 1000, 5000, 10000};
        int nArray[] = new int[]{100, 200, 300, 400, 500, 600,
                                 700, 800, 900, 1000, 1500, 2000, 2500, 3000, 4000, 5000};

        for (int N : nArray) {
            for (int numInts = 0; numInts < MAXNUMINTS; numInts++) {
                hugeA = new HugeInteger(N);
                hugeB = new HugeInteger(N);

                startTime = System.currentTimeMillis();

                for (int numRun = 0; numRun < MAXRUN; numRun++) {
                    hugeC = hugeA.multiply(hugeB);
                    //tmp = hugeA.compareTo(hugeB);
                }

                endTime = System.currentTimeMillis();
                runTimeHuge += (double) (endTime - startTime)/((double) MAXRUN);
            }
            runTimeHuge = runTimeHuge/((double) MAXNUMINTS);

            for (int numInts = 0; numInts < MAXNUMINTS; numInts++) {
                bigA = new BigInteger(new HugeInteger(N).toString());
                bigB = new BigInteger(new HugeInteger(N).toString());

                startTime = System.currentTimeMillis();

                for (int numRun = 0; numRun < MAXRUN; numRun++) {
                    bigC = bigA.multiply(bigB);
                    //tmp = bigA.compareTo(bigB);
                }

                endTime = System.currentTimeMillis();
                runTimeBig += (double) (endTime - startTime)/((double) MAXRUN);
            }
            runTimeBig = runTimeBig/((double) MAXNUMINTS);

            System.out.println(N + ", " + runTimeHuge + ", " + runTimeBig);
        }
    }
}

