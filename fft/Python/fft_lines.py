#!/usr/bin/python

import pygame
#from pygame.time import Clock#, delay
from numpy.fft import rfft
from numpy import logspace, log, e
#from numpy import array #, hanning#, logspace, log, e, floor

WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
RED = (255, 0, 0)
GREEN = (0, 255, 0)
circles = 100
CHUNKSIZE = 1024
width, height = 1024, 512
screen = pygame.display.set_mode((width, height))

ks = map(int, logspace(1, log(512), num=2048, endpoint=True, base=e))

def setup():
    pygame.display.set_caption('Audio Display Python Edition')
    screen.fill(WHITE)
    return None

def create_buffer():
    '''
    Reads in $CHUNKSIZE bytes from /tmp/mpd.fifo
    '''
    global stream
    chunk = stream.read(CHUNKSIZE*2)
    vals = [ord(i) for i in chunk[1::2]]
    return map(lambda x: 128-x if (x < 128) else 384 - x, vals)

def fft_draw(values):
    rect = pygame.Rect(0,0,0,0)
    rect.width = 2
    for i in range(1, width/2):
        rect.left = i*2
        rect.top = 512-values[i]
        rect.top = 256 if rect.top < 256 else rect.top
        rect.height = 512-rect.top
        pygame.draw.rect(screen, RED, rect)

def fft_draw2(xs, values):
    rect = pygame.Rect(0,0,0,0)
    rect.width = 2
    for i,x in enumerate(xs):
        rect.left = i/2
        rect.top = 512-values[x]
        rect.top = 256 if rect.top < 256 else rect.top
        rect.height = 512-rect.top
        pygame.draw.rect(screen, RED, rect)


def main():
    setup()
    running = True
    xs = [i for i in range(CHUNKSIZE)]
    while running:
        screen.fill(WHITE)
        ys = create_buffer()
        pointlist = zip(xs,ys)
        pygame.draw.lines(screen, RED, False, pointlist)

        ys_fft = (abs(rfft(ys))[1:])**2
        k = max(ys_fft)
        if k:
            ys_fft = map(lambda x: int(x/k*256), ys_fft)
        #fft_draw(ys_fft)
        fft_draw2(ks, ys_fft)

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
        pygame.display.flip()
        #delay(10)

if __name__ == "__main__":
    stream = open('/tmp/mpd.fifo', 'rb')
    main()
    print "Program quit."
    stream.close()

