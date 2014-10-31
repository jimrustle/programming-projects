use std::iter::range_step;
use std::f32::consts::PI;

// -- should be using fftw instead, but I don't know how to link C libs in Rust
// Adapted from C++
// Numerical Recipes. The Art of Scientific Computing, 3rd Edition, 2007
// ISBN 0-521-88068-8.

fn swap (data: &mut Box<[f32, .. 2048]>, i: uint, j:uint) {
    let tmp = data[j];
    data[j] = data[i];
    data[i] = tmp;
}

pub fn four1(data: &mut Box<[f32, .. 2048]>, nn: uint) {
    let n = nn << 1;
    let mut j = 1;

    for i in range_step(1, n, 2) {
        if j > i {
            swap(data, j-1, i-1);
            swap(data, j, i);
        }

        let mut m = nn;

        while m >= 2 && j>m {
            j -= m;
            m >>= 1;
        }
        j += m;
    }

    let mut mmax = 2;

    while n > mmax {
        let istep = mmax << 1;
        let theta = -(2.0 * PI/mmax as f32);
        let mut wtemp = (0.5 * theta).sin();
        let wpr = -2.0 * wtemp * wtemp;
        let wpi = theta.sin();
        let mut wr = 1.0;
        let mut wi = 0.0;
        for m in range_step(1, mmax, 2) {
            for i in range_step(m, n, istep) {
                j = i + mmax;
                let tempr = wr * data[j-1] - wi * data[j];
                let tempi = wr * data[j] + wi * data[j-1];

                data[j-1] = data[i-1] - tempr;
                data[j] = data[i] - tempi;
                data[i-1] += tempr;
                data[i] += tempi;
            }
            wtemp = wr;
            wr += wr * wpr - wi * wpi;
            wi += wi * wpr + wtemp * wpi;
        }
        mmax = istep;
    }
}

