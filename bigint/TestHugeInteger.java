
public class TestHugeInteger {
    static int MAXNUMINTS = 100;
    static int MAXRUN = 25;

    public static void main(String[] args) {
        HugeInteger A, B, C;
        A = new HugeInteger("123456789101112131415");
        B = new HugeInteger("900000000000000000000");

        System.out.println("\nTest 1:");
        System.out.println("A = " + A);
        System.out.println("B = " + B);
        System.out.println("A + B = " + A.add(B));
        System.out.println("B + A = " + B.add(A));
        System.out.println("A - B = " + A.subtract(B));
        System.out.println("B - A = " + B.subtract(A));
        System.out.println("A * B = " + A.multiply(B));
        System.out.println("A cmp B = " + A.compareTo(B));
        System.out.println("B cmp A = " + B.compareTo(A));

        A = new HugeInteger("-123456789101112131415");
        System.out.println("\nTest 2:");
        System.out.println("A = " + A);
        System.out.println("B = " + B);
        System.out.println("A + B = " + A.add(B));
        System.out.println("B + A = " + B.add(A));
        System.out.println("A - B = " + A.subtract(B));
        System.out.println("B - A = " + B.subtract(A));
        System.out.println("A * B = " + A.multiply(B));
        System.out.println("A cmp B = " + A.compareTo(B));
        System.out.println("B cmp A = " + B.compareTo(A));

        B = new HugeInteger("-900000000000000000000");
        System.out.println("\nTest 3:");
        System.out.println("A = " + A);
        System.out.println("B = " + B);
        System.out.println("A + B = " + A.add(B));
        System.out.println("B + A = " + B.add(A));
        System.out.println("A - B = " + A.subtract(B));
        System.out.println("B - A = " + B.subtract(A));
        System.out.println("A * B = " + A.multiply(B));
        System.out.println("A cmp B = " + A.compareTo(B));
        System.out.println("B cmp A = " + B.compareTo(A));

        A = new HugeInteger("-900000000000000000000");
        System.out.println("\nTest 4:");
        System.out.println("A = " + A);
        System.out.println("B = " + B);
        System.out.println("A + B = " + A.add(B));
        System.out.println("B + A = " + B.add(A));
        System.out.println("A - B = " + A.subtract(B));
        System.out.println("B - A = " + B.subtract(A));
        System.out.println("A * B = " + A.multiply(B));
        System.out.println("A cmp B = " + A.compareTo(B));
        System.out.println("B cmp A = " + B.compareTo(A));

        A = new HugeInteger("900000000000000000000");
        System.out.println("\nTest 5:");
        System.out.println("A = " + A);
        System.out.println("B = " + B);
        System.out.println("A + B = " + A.add(B));
        System.out.println("B + A = " + B.add(A));
        System.out.println("A - B = " + A.subtract(B));
        System.out.println("B - A = " + B.subtract(A));
        System.out.println("A * B = " + A.multiply(B));
        System.out.println("A cmp B = " + A.compareTo(B));
        System.out.println("B cmp A = " + B.compareTo(A));
    }
}

