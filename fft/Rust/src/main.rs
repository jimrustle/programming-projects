#![feature(phase)]
#[phase(plugin)]
extern crate gl_generator;
extern crate glfw;

use std::io::{File, IoResult};
use glfw::Context;
use std::f32::consts::PI;
use std::iter::range_step;
// use std::collections::{RingBuf, Deque};

mod gl {
    generate_gl_bindings!("gl", "core", "3.0", "static")
}

// -- should be using fftw instead, but I don't know how to link C libs in Rust
// Adapted from C++
// Numerical Recipes. The Art of Scientific Computing, 3rd Edition, 2007
// ISBN 0-521-88068-8.

fn swap (data: &mut Box<[f32, .. 2048]>, i: uint, j:uint) {
    let tmp = data[j];
    data[j] = data[i];
    data[i] = tmp;
}

fn four1(data: &mut Box<[f32, .. 2048]>, nn: uint) {
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

fn draw_spectrogram_line(x: f32, spec_line: &[f32]) {
    unsafe {
        gl::Begin(gl::QUAD_STRIP); // Top left, bottom left, bottom right, top right

        for y in range(0u, 256) {
            let y_pos = y as f32 + 256.0;
            let colorval = 1.0 - spec_line[y];
            //colorval = 1 - colorval;
            gl::Color3f(colorval, colorval, colorval);
            gl::Vertex2f(x, y_pos);
            gl::Vertex2f(x+1.0, y_pos);
        }

        gl::End();
    }
}

fn draw_spectrogram(start: uint, spectrogram: &Box<[f32, .. 256*512]>) {
    let mut address;
    for x in range(0u, 512) {
        address = (start + x) * 256;
        address %= 256 * 512;
        draw_spectrogram_line(x as f32, spectrogram.slice(address, address + 256));
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

fn draw_line_fft(array: &[f32]) {
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

fn draw_line_scope(array: &Box<[f32, .. 2048]>) {
    unsafe {
        gl::Begin(gl::LINE_STRIP);
        gl::Color3f(1.0, 0.0, 0.0);
        for i in range_step(0u, 2048, 2) {
            gl::Vertex2f(i as f32 / 2.0, array[i]);
        }
        gl::End();
    }
}

fn normalize(array: &mut [f32]) {
    let mut max = array[2];

    for i in range(3, 256) {
        if max < array[i] {
            max = array[i];
        }
    }

    if 0.0f32 < max {
        for i in range(0, 256) {
            array[i] /= max;
        }
    }
}

fn read_bytes(f: &mut IoResult<File>, buf: &mut Box<[f32, .. 2048]>) {
    for i in range_step(0u, 2048, 2) {
        match f.read_le_i16() {
            Ok(n) => {
                buf[i] = n as f32 / 256.0 + 128.0;
                buf[i+1] = 0.0f32;}
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
        gl::Disable(gl::ALPHA_TEST);
        gl::Disable(gl::BLEND);
        gl::Disable(gl::DEPTH_TEST);
        gl::Disable(gl::DITHER);
        gl::Disable(gl::FOG);
        gl::Disable(gl::LIGHTING);
        gl::Disable(gl::LOGIC_OP);
        gl::Disable(gl::STENCIL_TEST);
        gl::Disable(gl::TEXTURE_1D);
        gl::Disable(gl::TEXTURE_2D);
        gl::Ortho(0.0, 1024.0, 0.0, 512.0, 0.0, 1.0);
    }

    let mut signal = box [0.0f32, .. 2 * 1024];
    let mut spectrogram = box [0.0f32, .. 256 * 512];
    let mut start = 0;

    while !window.should_close() {
        unsafe {
            gl::ClearColor(1.0, 1.0, 1.0, 1.0);
            gl::Clear(gl::COLOR_BUFFER_BIT);
        }

        read_bytes(&mut file, &mut signal);
        draw_line_scope(&signal);

        four1(&mut signal, 1024);

        for i in range_step(0, 2 * 256, 2) {
            spectrogram[start*256 + i/2] = (signal[i].powi(2) + signal[i+1].powi(2)).sqrt();
        }

        draw_line_fft(spectrogram.slice(start * 256, start * 256 + 256));
        normalize(spectrogram.slice_mut(start * 256, start * 256 + 256));

        start += 1;
        start %= 512;

        draw_spectrogram(start, &spectrogram);

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
