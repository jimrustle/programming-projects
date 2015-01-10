
use std::iter::range_step;

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

fn draw_spectrogram_line(x: f32, spec_line: &[f32]) {
    unsafe {
        // Top left, bottom left, bottom right, top right
        gl::Begin(gl::QUAD_STRIP);

        //for (y, cv) in spec_line.iter().enumerate() {
        for y in range(0us, 256) {
            let y_pos = y as f32 + 256.0;
            //let colorval = 1.0 - *cv;
             let colorval = 1.0 - spec_line[y];
            //colorval = 1 - colorval;
            gl::Color3f(colorval, colorval, colorval);
            gl::Vertex2f(x, y_pos);
            gl::Vertex2f(x+1.0, y_pos);
        }

        gl::End();
    }
}

pub fn draw_spectrogram(start: usize, spectrogram: &Box<[f32; 256*512]>) {
    let mut address;
    unsafe {
    for x in range(0us, 512) {
        address = (start + x) * 256;
        address %= 256 * 512;
        draw_spectrogram_line(x as f32, spectrogram.slice(address,
                                                          address + 256));
    }
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

pub fn draw_line_fft(array: &[f32]) {
    let mut val;
    for i in range(1us, 256) {
        val = array[i]/20.0;
        val = if val < 256.0 {
            val
        } else {
            256.0
        };
        draw_rect(2.0 * i as f32, val);
    }
}

pub fn draw_line_scope(array: &Box<[f32;  2048]>) {
    unsafe {
        gl::Begin(gl::LINE_STRIP);
        gl::Color3f(1.0, 0.0, 0.0);
        for i in range_step(0us, 2048, 2) {
            gl::Vertex2f(i as f32 / 2.0, array[i]);
        }
        gl::End();
    }
}

pub fn normalize(array: &mut [f32]) {
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

