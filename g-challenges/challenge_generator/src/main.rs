use std::fs::File;
use std::io::Read;
use std::error::Error;
use std::vec::Vec;

extern crate rand;
use rand::Rng;

fn main() {
    // load languages file into Vec
    let file = match File::open("acceptable_languages.txt") {
        Err(e) => panic!("couldn't open /dev/urandom: {}", e.description()),
        Ok(file) => file,
    };

    let mut languages = Vec::new();
    let mut lang_str = String::new();

    for byte in file.bytes() {
        let b = byte.unwrap() as char;

        if b == '\n' {
            languages.push(lang_str.clone());
            lang_str.clear();
        } else {
            lang_str.push(b);
        }
    }

    println!("--- Programming challenges v4.0 generator ---");
    println!("Your language of choice for today's challenge is: {}!", rand::thread_rng().choose(&languages).unwrap());
    println!("Your challenge number is: {}!", rand::thread_rng().gen_range(0, 146));

}
