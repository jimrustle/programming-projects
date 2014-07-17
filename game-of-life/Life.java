public class Life {
    static final int SIZE = 20;

    public static int[] shiftup(int array[]) {
      int[] ret = new int[SIZE*SIZE];

      for (int i = 0; i < SIZE*(SIZE - 1); i++) {
        ret[i] = array[i+SIZE];
      }

      for (int i = 0; i < SIZE; i++) {
        ret[SIZE*(SIZE - 1)+i] = array[i];
      }

      return ret;
    }

    public static int[] shiftdown(int array[]) {
      int[] ret = new int[SIZE*SIZE];

      for (int i = 0; i < SIZE; i++) {
        ret[i] = array[SIZE*(SIZE - 1) + i];
      }

      for (int i = 0; i < SIZE*(SIZE - 1); i++) {
        ret[SIZE+i] = array[i];
      }

      return ret;
    }

    public static int[] shiftleft(int array[]) {
        int[] ret = new int[SIZE*SIZE];
        int[] old = new int[SIZE];
        for (int i = 0; i < SIZE*SIZE; i+=SIZE) {
            old[i/SIZE] = array[i];
        }

        for (int i = 0; i < SIZE*SIZE - 1; i++){
            ret[i] = array[i+1];
        }

        for (int i = SIZE - 1; i < SIZE*SIZE; i+=SIZE) {
            ret[i] = old[i/SIZE];
        }

        return ret;
    }

    public static int[] shiftright(int array[]) {
        int[] ret = new int[SIZE*SIZE];
        int[] old = new int[SIZE];
        for (int i = SIZE - 1; i < SIZE*SIZE; i+=SIZE) {
            old[i/SIZE] = array[i];
        }

        for (int i = 1; i < SIZE*SIZE; i++){
            ret[i] = array[i-1];
        }

        for (int i = 0; i < SIZE*SIZE; i+=SIZE) {
            ret[i] = old[i/SIZE];
        }

        return ret;
    }

    public static int[] add_arrays(int a[], int b[], int c[], int d[],
                                   int e[], int f[], int g[], int h[]) {
        int[] k = new int[SIZE*SIZE];

        for (int i = 0; i < SIZE*SIZE; i++) {
            k[i] = a[i] + b[i] + c[i] + d[i] + e[i] + f[i] + g[i] + h[i];
        }

        return k;
    }

    public static int[] next_state(int old[]){
        int[] next = new int[SIZE*SIZE];

        next = add_arrays(shiftup(old),
                          shiftdown(old),
                          shiftleft(old),
                          shiftright(old),
                          shiftup(shiftleft(old)),
                          shiftup(shiftright(old)),
                          shiftdown(shiftleft(old)),
                          shiftdown(shiftright(old)));

        for (int i = 0; i < SIZE*SIZE; i++) {
            if (((old[i] == 1) && (next[i] == 2)) || (next[i] == 3)) {
                next[i] = 1;
            }
            else {
                next[i] = 0;
            }
        }

        return next;
    }

    public static void main(String args[]) {
        int[] life = new int[SIZE*SIZE];
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                life[SIZE*i+j] = 0;
            }
        }

        life[89] = 1;
        life[90] = 1;
        life[91] = 1;
        life[93] = 1;
        life[109] = 1;
        life[132] = 1;
        life[133] = 1;
        life[150] = 1;
        life[151] = 1;
        life[153] = 1;
        life[169] = 1;
        life[171] = 1;
        life[173] = 1;

        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                System.out.print(life[SIZE*i+j] + " ");
            }
            System.out.println();
        }

        System.out.println("---");

        life = next_state(life);

        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                System.out.print(life[SIZE*i+j] + " ");
            }
            System.out.println();
        }
    }
}
