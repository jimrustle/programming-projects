## MPD audio displays

![Lisp MPD Visualizer](fft.png "Lisp MPD Visualizer")

### Write-up
A completely experimental and educational experience (for me, anyway)
on digital signal processing, graphics, and learning new languages and APIs.

All of the implementations follow the basic idea, they open the named pipe,
/tmp/mpd.fifo, and read in 1024 16-bit samples from the audio stream.

From these samples,

1. Oscilloscope - A line strip is drawn to the screen using the values
2. Frequency spectrum - An FFT is calculated using the values, and is drawn
3. Spectrogram - The FFT is stored, and is drawn to the screen as a history


### Implementations

- C and C++ implementation using FFTW3 and glfw3 + OpenGL
- Commmon Lisp - bordeaux-fft, cl-opengl and cl-glfw3
- Haskell
- Java - jglfw, and meapsoft's fft library
- Lua - luajit-glfw and luajit-fftw3
- OCaml - batteries, fftw, lablgl, glut
- Matlab - not a full implementation
- Python - pyglet, numpy
- Rust - doesn't build anymore
- Scheme - unfinished

# Licensing
Public domain.
C and Lua implementations should be GPL2 as they use FFTW.
Java implementation is also GPL2, as Meapsoft code is used.

