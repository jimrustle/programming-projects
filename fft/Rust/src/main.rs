extern crate glfw;
extern crate mpd_display;

use mpd_display::fft::four1;
use mpd_display::read::read_bytes;
use mpd_display::draw;
use std::io::File;
use glfw::{Action, Context, Key};
use std::num::Float;
use std::iter::range_step;

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
    //glfw.window_hint(glfw::Context::ContextVersion(3, 0));

    let (window, events) = glfw.create_window(1024, 512, "Rust MPD Display",
                                              glfw::WindowMode::Windowed)
        .expect("Failed to create GLFW window.");


    window.set_key_polling(true);
    window.make_current();

    draw::gl::load_with(|s| window.get_proc_address(s));

    draw::gl_glfw_initialise();

    let mut signal = Box::new([0.0f32; 2 * 1024]);
    let mut spectrogram = Box::new([0.0f32; 256 * 512]);
    let mut start = 0;

    while !window.should_close() {
        draw::clear_screen();

        read_bytes(&mut file, &mut signal);

        draw::draw_line_scope(&signal);

        four1(&mut signal, 1024);

        unsafe{
        for i in range_step(0, 2 * 256, 2) {
            spectrogram[start*256 + i/2] = (signal[i].powi(2) +
                                            signal[i+1].powi(2)).sqrt();
        }
        }

        draw::draw_line_fft(spectrogram.slice(start * 256, start * 256 + 256));
        draw::normalize(spectrogram.slice_mut(start * 256, start * 256 + 256));

        start += 1;
        start %= 512;

        draw::draw_spectrogram(start, &spectrogram);

        window.swap_buffers();
        glfw.poll_events();
        for (_, event) in glfw::flush_messages(&events) {
            match event {
                glfw::WindowEvent::Key(Key::Q, _, Action::Press, _) => {
                    window.set_should_close(true)
                }
                _ => {}
            }
        }
    }

    println!("Program quit.");
}

