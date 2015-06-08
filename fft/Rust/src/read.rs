extern crate byteorder;

use std::fs::File;
use self::byteorder::{LittleEndian, ReadBytesExt};

pub fn read_file_get_vec(f: &mut File) -> Vec<f32> {
    let mut v : Vec<f32> = Vec::with_capacity(1024);
    for _ in 0 .. 1024 {
        v.push(f.read_i16::<LittleEndian>().unwrap() as f32 / 256.0 + 128.0);
    }
    v
}
