use std::io::{File, IoResult};
use std::iter::range_step;

pub fn read_bytes(f: &mut IoResult<File>, buf: &mut Box<[f32; 2048]>) {
    for i in range_step(0us, 2048, 2) {
        match f.read_le_i16() {
            Ok(n) => {
                buf[i] = n as f32 / 256.0 + 128.0;
                buf[i+1] = 0.0f32;}
            Err(e) => println!("Error reading: {}", e)
        }
    }
}

