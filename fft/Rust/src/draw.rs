
use std::collections::VecDeque;

pub mod gl {
    include!(concat!(env!("OUT_DIR"), "/gl_bindings.rs"));
}

pub fn clear_screen() {
    unsafe {
        gl::ClearColor(1.0, 1.0, 1.0, 1.0);
        gl::Clear(gl::COLOR_BUFFER_BIT);
    }
}

pub fn gl_glfw_initialise() {
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
}

fn draw_spectrogram_line(x: f32, spec_line: &Vec<f32>) {
    unsafe {
        //// Top left, bottom left, bottom right, top right
        gl::Begin(gl::QUAD_STRIP);

        ////for (y, cv) in spec_line.iter().enumerate() {
        for y in 0 .. 256 {
            let y_pos = y as f32 + 256.0;
            ////let colorval = 1.0 - *cv;
            let colorval = 1.0 - spec_line[y];
            ////colorval = 1 - colorval;
            gl::Color3f(colorval, colorval, colorval);
            gl::Vertex2f(x, y_pos);
            gl::Vertex2f(x+1.0, y_pos);
        }

        gl::End();
    }
}

pub fn draw_spectrogram(spectrogram: &VecDeque<Vec<f32>>) {
    for (i, x) in spectrogram.iter().enumerate() {
        draw_spectrogram_line(i as f32, x);
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

pub fn draw_line_fft(vec: &Vec<f32>) {
    let mut val;
    for i in 1 .. 256 {
        val = vec[i]/20.0;
        val = if val < 256.0 {
            val
        } else {
            256.0
        };
        draw_rect((2 * i) as f32, val);
    }
}

pub fn draw_line_scope(vec: &Vec<f32>) {
    unsafe {
        gl::Begin(gl::LINE_STRIP);
        gl::Color3f(1.0, 0.0, 0.0);
        for i in 0 .. 1024 {
            gl::Vertex2f(i as f32, vec[i]);
        }
        gl::End();
    }
}

