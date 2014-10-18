#![feature(phase)]
#[phase(plugin)]
extern crate gl_generator;
extern crate glfw;

use std::io::{File, IoResult};
use glfw::Context;
use std::f32::consts::PI;
use std::iter::range_step;
use std::collections::{RingBuf, Deque};

mod gl {
    generate_gl_bindings!("gl", "core", "3.0", "static")
}

// Adapted from C++
// Numerical Recipes. The Art of Scientific Computing, 3rd Edition, 2007
// ISBN 0-521-88068-8.

fn swap (data: &mut Vec<f32>, i: uint, j:uint) {
    let a = (*data)[i];
    let b = (*data)[j];

    *data.get_mut(i) = b;
    *data.get_mut(j) = a;
}

fn four1(data: &mut Vec<f32>, nn: uint) {
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
                let tempr = wr * (*data)[j-1] - wi * (*data)[j];
                let tempi = wr * (*data)[j] + wi * (*data)[j-1];

                *data.get_mut(j-1) = (*data)[i-1] - tempr;
                *data.get_mut(j) = (*data)[i] - tempi;
                *data.get_mut(i-1) += tempr;
                *data.get_mut(i) += tempi;
            }
            wtemp = wr;
            wr += wr * wpr - wi * wpi;
            wi += wi * wpr + wtemp * wpi;
        }
        mmax = istep;
    }
}

fn draw_spectrogram_line(x: f32, spec_line: &Vec<f32>) {
    unsafe {
        gl::Begin(gl::QUAD_STRIP); // Top left, bottom left, bottom right, top right

    for y in range(0u, 256) {
        let y_pos = y as f32+ 256.0;
        let colorval = 1.0 - spec_line[y];
        //colorval = 1 - colorval;
        gl::Color3f(colorval, colorval, colorval);
        gl::Vertex2f(x, y_pos);
        gl::Vertex2f(x+1.0, y_pos);
    }

        gl::End();
    }
}

fn draw_spectrogram(spectrogram: &RingBuf<Vec<f32>>) {
    for (i, spec_line) in range(0f32, 512.0).zip(spectrogram.iter()) {
        draw_spectrogram_line(i, spec_line);
    }
}

fn draw_rect(mut x: f32, mut y: f32) {
    unsafe {
        gl::Begin(gl::QUADS);
        gl::Color3f(1.0, 0.0, 0.0);

    y += 256.0;
    x += 512.0;

        gl::Vertex2f(x, y);
        gl::Vertex2f(x, 256.0);
        gl::Vertex2f(x+1.0, 256.0);
        gl::Vertex2f(x+1.0, y);
        gl::End();
    }
}

fn draw_line_fft(array: &Vec<f32>) {
    let mut val;
    for i in range(1u, 256) {
        val = array[i]/20.0;
        val = if val < 256.0 {
            val
        } else {
            256.0
        };
        draw_rect(2.0 * i as f32, val);
    }
}

fn draw_line_scope(array: &Vec<f32>) {
    unsafe {
        gl::Begin(gl::LINE_STRIP);
        gl::Color3f(1.0, 0.0, 0.0);
        for i in range_step(0u, 2048, 2) {
            gl::Vertex2f(i as f32 / 2.0, array[i]);
        }
        gl::End();
    }
}

fn read_bytes(f: &mut IoResult<File>, buf: &mut Vec<f32>) {
    buf.clear();
    for i in range(0u, 1024) {
        match f.read_le_i16() {
            Ok(n) => {
                buf.push(n as f32 / 256.0 + 128.0);
                buf.push(0.0f32)},
            Err(e) => println!("Error reading: {}", e)
        }
    }
}


fn main() {
    let mut file = File::open(&Path::new("/tmp/mpd.fifo"));

    match file {
        Ok(_) => (),
        Err(_) => {println!("Error opening /tmp/mpd.fifo -- is mpd running?");
            std::os::set_exit_status(1);
            return;
        }
    }

    let glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
    glfw.window_hint(glfw::ContextVersion(3, 0));

    let (window, events) = glfw.create_window(1024, 512, "Rust MPD Display", glfw::Windowed)
        .expect("Failed to create GLFW window.");

    window.set_key_polling(true);
    window.make_current();

    unsafe {
        gl::Ortho(0.0, 1024.0, 0.0, 512.0, 0.0, 1.0);
    }

    let mut buf = Vec::with_capacity(2 * 1024);
    let mut spectrogram : RingBuf<Vec<f32>> = RingBuf::with_capacity(512);

    //spectrogram.reserve_exact(512);
    for i in range(0u, 512) {
        spectrogram.push(Vec::from_fn(512, |_| 0.0f32));
    }

    while !window.should_close() {
        let mut fft_out = Vec::with_capacity(256);
        unsafe {
            gl::ClearColor(1.0, 1.0, 1.0, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);
        }

        buf.clear();
        read_bytes(&mut file, &mut buf);
        draw_line_scope(&buf);

        four1(&mut buf, 1024);

        for i in range_step(0, 2 * 256, 2) {
            fft_out.push((buf[i].powi(2) + buf[i+1].powi(2)).sqrt());
        }

        draw_line_fft(&fft_out);

        spectrogram.pop_front();

        let mut max = 0.0f32;

        for v in fft_out.iter() {
            if max < *v {
                max = *v;
            }
        }

        max /= 100.0;

        if 0.0f32 < max {
            spectrogram.push(fft_out.iter().map(|x| x/max ).collect());
        }
        else {
            spectrogram.push(fft_out);
        }

        draw_spectrogram(&spectrogram);

        window.swap_buffers();
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            match event {
                glfw::KeyEvent(glfw::KeyQ, _, glfw::Press, _) => {
                    window.set_should_close(true)
                }
                _ => {}
            }
        }
    }
}
