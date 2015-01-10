extern crate gl_generator;    // <-- this is your build dependency
extern crate khronos_api;    // included by gl_generator

use std::os;
use std::io::File;

fn main() {
    let dest = Path::new(os::getenv("OUT_DIR").unwrap());

    let mut file = File::create(&dest.join("gl_bindings.rs")).unwrap();

    // This generates bindsings for OpenGL ES v3.1
    gl_generator::generate_bindings(gl_generator::GlobalGenerator,
                                    gl_generator::registry::Ns::Gl,
                                    khronos_api::GL_XML,
                                    vec![],
                                    "3.0", "core", &mut file).unwrap();
}
