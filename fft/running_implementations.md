## Running Implementations

If you're interested in running these programs, here's a quick run-down of how I
did it.

# C
The dependencies for C include:
- ~~SDL, as libsdl1.2-dev~~
- ~~glfw, as libglfw2, as of 26 August~~
- glfw3 as of 03 September
- fftw, as libfftw3-dev

To compile, run ```make```, then ```./fft```.

# Python
- numpy, for the FFT and array operations
- pygame 1.9.1, installed from the package manager
- pyglet 1.1.4, installed from pip

To run, run the Python interpreter on the file.

# Lua
- ~~LÖVE 0.9.0, installed from the package manager~~
- ~~luafft - I also installed this one side-by-side in the source directory~~
~~since LuaRocks didn't want to play nicely and install anything~~
~~To run, run ```love .``` in the directory.~~

Now uses:
- luajit-glfw (glfw3)
- luajit-fftw

Run by running ``luajit`` on the file. Make sure luajit-glfw and fftw are
installed properly.

# Matlab
- It's actually Octave, but just tap ```C-C```, or ```^C``` to quit
- It doesn't quit correctly (and won't be fixed)
- Run the interpreter on the file as like any other Matlab/Octave file

# Lisp
- quicklisp "lispbuilder-sdl", "bordeaux-fft" and "cl-opengl"
- Run it through emacs/slime/sbcl
