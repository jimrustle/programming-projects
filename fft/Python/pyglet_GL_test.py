from pyglet.gl import *
from music_functions import create_buffer
#from numpy.fft import rfft
#from numpy import array

stream = open('/tmp/mpd.fifo', 'rb')
window = pyglet.window.Window(width=1024, height=512, resizable=False) #, vsync=False)
window.set_caption("MPD Audio Display Pyglet/Python Edition")
glClearColor(255, 255, 255, 0)

def draw_scope(ys):
    glBegin(GL_LINE_STRIP)
    for i in range(1024):
        glColor3f(1, 0, 0)
        glVertex2f(i, 256+ys[i])
    glEnd()

def draw_fft(ys):
    for i in range(512):
        draw_rect(i, ys[i])

def draw_rect(x, y):
    glBegin(GL_TRIANGLES)
    glColor3f(1, 0, 0)
    glVertex2f(x, 0)
    glColor3f(1, 0, 0)
    glVertex2f(x, y)
    glColor3f(1, 0, 0)
    glVertex2f(x+4, 0)
    glEnd()
    glBegin(GL_TRIANGLES)
    glColor3f(1, 0, 0)
    glVertex2f(x, y)
    glColor3f(1, 0, 0)
    glVertex2f(x+4, y)
    glColor3f(1, 0, 0)
    glVertex2f(x+2, 0)
    glEnd()

@window.event
def on_draw():
    window.clear()
    glClear(GL_COLOR_BUFFER_BIT)
    glLoadIdentity()
    ys = create_buffer(stream)
    draw_scope(ys)

    #ys_fft = abs(rfft(ys))[1:]
    ##ys_fft = array(map(int, ys_fft))
    #k = max(ys_fft[10:])
    #if k:
        #ys_fft = map(lambda x: int(x/k*256), ys_fft)
    #print ys_fft

    #draw_fft(ys_fft)

pyglet.clock.schedule_interval(lambda dt: None, 1/120.0)
pyglet.app.run()

print "Program quit."
stream.close()
