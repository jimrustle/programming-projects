## MPD audio displays

### Write-up
A completely experimental and educational experience (for me, anyway)
on digital signal processing, graphics, and learning new languages and APIs.

All of the implementations follow the basic idea, they open the named pipe,
/tmp/mpd.fifo, and read in 2048 or bytes individually (!) from the audio
stream. From this input, every other byte is dropped, and only the even
indexes are kept as data.
(This happens in the SDL fft implementations. For the opengl implementation, it
is *far* faster, as it uses a file descriptor instead)

The input data is then drawn to the screen after being shuffled through a math
function to centre it around 256, and then a FFT is performed on the data.
Then, this data is drawn to the screen.

In the Python/pygame implementation, I tried to make the FFT display with
logarithmic bins, just to see what it would look like.

Oh, and as a final note, I have no idea if comments are up to date in any of
the programs. I think the Lua implementation is the cleanest, and the Python
implementation the most explanatory.

### Implementations
- C - two implementations, one uses kiss\_fft and the other uses fftw, both use
SDL for graphics
- C now has a (bad) OpenGL implementation using fftw
- Python - two implementations, one uses pygame and the other uses pyglet.
Both of the implementations use the same FFT library, numpy.
The pyglet implementation is currently unfinished (purely because the OpenGL
book is so difficult to read).
- Lua - uses LÖVE 0.9.0 and luafft
- Scheme - in the works, planning to use guile-2d and a currently undecided fft library

### Benchmarks
I thought it would be nice to include (completely non-scientific) CPU loads
I measured while running each implementation.

- C - ~8% for both implementations
- Python - ~81% for the pygame implementation, pyglet takes about as much, but with pypy it's around 20%
- Lua - ~75%


# Licensing
Honestly, I haven't taken the time to learn licensing rules, but I'm aware that any
project that uses FFTW must be GPL'd.

I think that technically makes the C-fftw implementation GPL, but I'm not sure
how it affects the other ones.
