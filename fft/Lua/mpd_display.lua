
local lj_glfw = require "glfw"
local ffi = require "ffi"
local gl, glc, glu, glfw, glext = lj_glfw.libraries()
require 'luarocks.loader'
local fftw = require 'fftw3'

function read_buffer(data, stream)
    for x = 0, 1023 do
        stream:read(1)
        data[x] = (string.byte(stream:read(1)) + 128) % 256
    end
end

function draw_spectrogram(spectrogram)
    for i=1,512 do
        draw_spec_line(i-1, spectrogram)
    end
end

function draw_spec_line(x, spectrogram)
    gl.glBegin(glc.GL_QUAD_STRIP)
    for y=1,256 do
        colorval = spectrogram[(x*256)+y]
        colorval = 1 - colorval
        gl.glColor3f(colorval, colorval, colorval)
        local y_pos = y + 256
        gl.glVertex2f(x, y_pos)
        gl.glVertex2f(x+1, y_pos)
    end
    gl.glEnd()
end

function draw_line_scope(data)
    gl.glColor3d(1.0, 0.0, 0.0)
    gl.glBegin(glc.GL_LINE_STRIP)
    for i=1,1023 do
        gl.glVertex2f(i, data[i])
    end
    gl.glEnd()
end

function shift_spectrogram(spectrogram, new)
    for i=256+1,256*512 do
        spectrogram[i-256] = spectrogram[i]
    end
    for i=1, 256 do
        spectrogram[i+256*511] = new[i]
    end
end

function normalise(data)
    k = math.max(unpack(data, 2))
    if (0 < k) then
        for x=1,256 do
            data[x] = data[x]/k
        end
    end
    return data
end

function draw_rect(x, y)
    gl.glBegin(glc.GL_QUADS)
    y = y + 256
    x = x + 512
    gl.glVertex2f(x, y)
    gl.glVertex2f(x, 256)
    gl.glVertex2f(x+1, 256)
    gl.glVertex2f(x+1, y)

    gl.glEnd()
end

function draw_line_fft(power)
    for i = 1,256 do
        draw_rect(2*i-1, power[i]);
    end
end

function magnitude(out, draw_output)
    for i=1,512 do
        draw_output[i] = math.sqrt(out[i-1][0] * out[i-1][0]
                            + out[i-1][1] * out[i-1][1])/20.0
    end
end

lj_glfw.init()
local window = lj_glfw.Window(1024, 512, "MPD Visualizer")

window:setKeyCallback(
function(window, key)
    if key == string.byte('Q') then
        window:setShouldClose(glc.GL_TRUE)
    end
end)
window:makeContextCurrent()

gl.glOrtho(0, 1024, 0, 512, 0, 1);
gl.glClearColor(1.0, 1.0, 1.0, 0);

stream = io.open("/tmp/mpd.fifo", "rb")

local data = ffi.new("double [1024]", 1024)
local fft_cplx_out = ffi.new("double [1024][2]")
local p = fftw.plan_dft_r2c_1d(1024, data, fft_cplx_out, 0)
local spectrogram = {}
local fft_draw_out = {}

for i=1,256*512+1 do
    spectrogram[i] = 0
end

while not window:shouldClose() do
    gl.glClear(glc.GL_COLOR_BUFFER_BIT)

    read_buffer(data, stream)
    fftw.execute(p)

    magnitude(fft_cplx_out, fft_draw_out)

    draw_line_scope(data)
    draw_line_fft(fft_draw_out)

    shift_spectrogram(spectrogram, normalise(fft_draw_out))
    draw_spectrogram(spectrogram)

    lj_glfw.pollEvents()
    window:swapBuffers()
end

io.close(stream)
window:destroy()
lj_glfw.terminate()
