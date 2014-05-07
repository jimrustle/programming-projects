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

# Python
- numpy, for the fft
- pygame
- pyglet

# Lua
- LÖVE
- luafft - I also installed this one side-by-side in the source directory
since LuaRocks didn't want to play nicely and install anything
