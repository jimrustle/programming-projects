extern crate glfw;
extern crate mpd_display;

use mpd_display::fft;
use mpd_display::read;
use mpd_display::draw;
use std::fs::File;
use std::collections::VecDeque;
use glfw::{Action, Context, Key};

fn main() {
    let mut file = match File::open("/tmp/mpd.fifo") {
        Ok(f) => f,
        Err(_) => {println!("Error opening /tmp/mpd.fifo -- is mpd running?");
            std::process::exit(1);
        }
    };

    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
    //glfw.window_hint(glfw::Context::ContextVersion(3, 0));

    let (mut window, events) = glfw.create_window(1024, 512,
                                                  "Rust MPD Display",
                                                  glfw::WindowMode::Windowed)
        .expect("Failed to create GLFW window.");


    window.set_key_polling(true);
    window.make_current();

    //draw::gl::load_with(|s| window.get_proc_address(s));

    draw::gl_glfw_initialise();

    let mut spectrogram = VecDeque::with_capacity(512);
    println!("Spectrogram capacity: {}", spectrogram.capacity());
    println!("Spectrogram length: {}", spectrogram.len());

    for _ in (0..512) {
        let mut blank_vec = Vec::with_capacity(256);
        for _ in (0..256) {
            blank_vec.push(0f32);
        }
        spectrogram.push_back(blank_vec);
    }

    println!("Spectrogram capacity: {}", spectrogram.capacity());
    println!("Spectrogram length: {}", spectrogram.len());

    while !window.should_close() {
        draw::clear_screen();

        let signal = read::read_file_get_vec(&mut file);

        draw::draw_line_scope(&signal);

        let fft_signal = fft::four1(&signal, 1024);
        draw::draw_line_fft(&fft_signal);

        let norm_fft = fft::normalise(&fft_signal);
        spectrogram.pop_front();
        spectrogram.push_back(norm_fft);
        draw::draw_spectrogram(&spectrogram);

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
    std::process::exit(0)
}

