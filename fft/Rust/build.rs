
extern crate gl_generator;    // <-- this is your build dependency
extern crate khronos_api;    // included by gl_generator

use std::os;
use std::fs::File;
use std::path::Path;

fn main() {
    let dir = std::env::var("OUT_DIR").unwrap();
    let dest = Path::new(&dir);

    let mut file = File::create(&dest.join("gl_bindings.rs")).unwrap();

    // This generates bindsings for OpenGL ES v3.1
    gl_generator::generate_bindings(gl_generator::StaticGenerator,
                                    gl_generator::registry::Ns::Gl,
                                    gl_generator::registry::Fallbacks::None,
                                    khronos_api::GL_XML,
                                    vec![],
                                    "3.1", "compatibility", &mut file).unwrap();
}

