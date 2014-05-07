function love.load()
    love.graphics.setBackgroundColor(255, 255, 255)
    love.graphics.setColor(255, 0, 0)
    --love.graphics.setLineStyle("smooth")
    love.graphics.setLineStyle("rough")
    --love.graphics.setLineWidth(1)
    love.window.setTitle("Audio Visualiser Lua Edition")
    love.window.setMode(1024, 512)
end

function fn(x)
    return 128 > x and 128-x or 384-x
end

function read_buffer(data, stream)
    for x = 1, 1024 do
        stream:read(1)
        data[x] = fn(string.byte(stream:read(1)))
        --io.write(t[x], '\n')
    end
    --io.write(string.byte(t))
end

function fft_intensity(data)
    ret = {}
    for x = 1, 1024 do
        ret[x] = math.sqrt(math.pow(data[x][1], 2) + math.pow(data[x][2], 2))/20
    end
    return ret
end

-- its like main, but worse

luafft = require "luafft"
require "math"

function love.draw()
    stream = io.open("/tmp/mpd.fifo", "rb")
    data = {}
    read_buffer(data, stream)
    for i=1, 1023 do   -- loop through all of our stars
        --io.write(data[i], '\n');
        --point = f(data[i])
        --point2 =  f(data[i+1])
        --love.graphics.line(i+0.5,point, i+1.5,point2)
        love.graphics.line(i+0, data[i], i+1, data[i+1])
    end
    ret = fft(data, false)
    vals = fft_intensity(ret)
    for i = 2, 512 do
        --io.write(vals[i], "\n")
        yval = 512 - vals[i]
        yval = yval < 256 and 256 or yval
        height = 512 - yval
        love.graphics.rectangle("fill", 2*(i-1), yval, 2, height)
    end
    stream:close()
end
