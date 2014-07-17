/*
 * Any live cell with fewer than two live neighbours dies, as if caused by
 * under-population.
 * Any live cell with two or three live neighbours lives on to the next generation.
 * Any live cell with more than three live neighbours dies, as if by overcrowding.
 * Any dead cell with exactly three live neighbours becomes a live cell, as
 * if by reproduction.
 */

static final int SIZE = 20;
int[] life = new int[SIZE*SIZE];

int[] shiftup(int array[]) {
  int[] ret = new int[SIZE*SIZE];

  for (int i = 0; i < SIZE*(SIZE - 1); i++) {
    ret[i] = array[i+SIZE];
  }

  for (int i = 0; i < SIZE; i++) {
    ret[SIZE*(SIZE - 1)+i] = array[i];
  }

  return ret;
}

int[] shiftdown(int array[]) {
  int[] ret = new int[SIZE*SIZE];

  for (int i = 0; i < SIZE; i++) {
    ret[i] = array[SIZE*(SIZE - 1) + i];
  }

  for (int i = 0; i < SIZE*(SIZE - 1); i++) {
    ret[SIZE+i] = array[i];
  }

  return ret;
}

int[] shiftleft(int array[]) {
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

int[] shiftright(int array[]) {
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

int[] add_arrays(int a[], int b[], int c[], int d[],
    int e[], int f[], int g[], int h[]) {
  int[] k = new int[SIZE*SIZE];

  for (int i = 0; i < SIZE*SIZE; i++) {
    k[i] = a[i] + b[i] + c[i] + d[i] + e[i] + f[i] + g[i] + h[i];
  }

  return k;
}

int[] next_state(int old[]){
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

void setup() {
  rectMode(RADIUS);
  frameRate(5);
  size(735, 735);
  background (0, 0, 0);

  for (int i = 0; i < SIZE; i++) {
    for (int j = 0; j < SIZE; j++) {
      life[SIZE*i+j] = (int) random(0, 2);
    }
  }

  //life[15] = 1;
  //life[17] = 1;
  //life[36] = 1;
  //life[37] = 1;
  //life[56] = 1;
}

//void draw_state(int state[]) {
//}

void draw() {
  for (int i = 0; i < SIZE; i++) {
    for (int j = 0; j < SIZE; j++) {
      if (life[SIZE*i+j] == 1) {
        fill(255, 0, 0);
      }
      else {
        fill(255, 255, 255);
      }

      ellipse(35+35*j, 35*i+35, 35, 35);
    }
  }
  life = next_state(life);
}
