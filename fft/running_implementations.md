## Running Implementations

If you're interested in running these programs, here's a quick run-down of how I
did it.

# C
The dependencies for C include:
- SDL, as libsdl1.2-dev
- fftw, as libfftw3-dev
- kiss\_fft, which was compiled and installed into $HOME/.local/include
I also left the kiss\_fftr.c and kiss\_fft.c in the same directory as
the source files as the visualiser. (since I never really learned how
to compile and link files correctly)

To compile, run make, then ./fft_sdl or ./fft_sdl_fftw.

# Python
- numpy, for the fft
- pygame 1.9.1, installed from the package manager
- pyglet 1.1.4, installed from pip

To run, run the Python interpreter on the file.

# Lua
- LÖVE 0.9.0, installed from the package manager
- luafft - I also installed this one side-by-side in the source directory
since LuaRocks didn't want to play nicely and install anything

To run, run ```love .``` in the directory.

# Matlab
- It\'s actually Octave, but just tap ```C-C```, or ```^C``` to quit
- Run the interpreter on the file as like any other Matlab/Octave file
