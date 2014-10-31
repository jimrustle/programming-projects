## MPD audio displays

![Lisp MPD Visualizer](fft.png "Lisp MPD Visualizer")

### Write-up
A completely experimental and educational experience (for me, anyway)
on digital signal processing, graphics, and learning new languages and APIs.

All of the implementations follow the basic idea, they open the named pipe,
/tmp/mpd.fifo, and read in 1024 16-bit samples from the audio stream.

The input data is then drawn to the screen after being shuffled through a math
function to centre it around 256, and then a FFT is performed on the data.
Then, this data is drawn to the screen.

There's also a spectrogram. Only the C and Common Lisp implementations are
fully complete.

### Implementations
- C implementation using FFTW3 and ~~SDL~~ glfw + OpenGL
(former C implementations removed -- view commit history)
- Python - two implementations, one uses pygame and the other uses pyglet.
Both of the implementations use the same FFT library, numpy.
The pyglet implementation is currently unfinished, out of laziness
- Lua - ~~uses LÖVE 0.9.0 and luafft~~
uses luajit-glfw and luajit-fftw3
- Commmon Lisp - cl-opengl and lispbuilder-sdl
- Scheme - in the works, planning to use guile-2d and a currently undecided fft library
- Matlab for fun (warning: major bugs)
- Rust - it works now! if only it was as fast as C/C++/LuaJIT

# Licensing
Public domain.
C and Lua implementations should be GPL2 as it uses FFTW.

