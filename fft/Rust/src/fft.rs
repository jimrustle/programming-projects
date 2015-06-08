use std::f32::consts::PI;

// -- should be using fftw instead, but I don't know how to link C libs in Rust
// Adapted from C++
// Numerical Recipes. The Art of Scientific Computing, 3rd Edition, 2007
// ISBN 0-521-88068-8.

fn swap (data: &mut [f32; 2048], i: usize, j:usize) {
    let tmp = data[j];
    data[j] = data[i];
    data[i] = tmp;
}

pub fn four1(input_signal: &Vec<f32>, nn: usize) -> Vec<f32> {
    let n = nn << 1;
    let mut j = 1;
    let mut data =  [0f32; 2048];
    let mut ret = Vec::with_capacity(1024);

    for i in (1..1024) {
        data[2*i] = input_signal[i];
    }

    for i in (1..n).step_by(2) {
        if j > i {
            swap(&mut data, j-1, i-1);
            swap(&mut data, j, i);
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
        for m in (1..mmax).step_by(2) {
            for i in (m..n).step_by(istep) {
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

    for i in (0..256) {
        ret.push((data[i] * data[i] + data[i+1] * data[i+1]).sqrt());
    }

    ret
}

pub fn normalise(vec: &Vec<f32>) -> Vec<f32> {
    let mut max = vec[2];
    let mut ret = Vec::with_capacity(256);

    for i in 3 .. 256 {
        if max < vec[i] {
            max = vec[i];
        }
    }

    if 0.1f32 < max {
        for i in 0 .. 256 {
            ret.push(vec[i]/max);
        }
    }
    ret
}
