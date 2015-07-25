import Tkinter
import socket
from rocket import Rocket
from math import sin, cos, pi

HOST_ADDR = '127.0.0.1'
HOST_PORT = 42069
WIN_X = 800
WIN_Y = 600

root = Tkinter.Tk()
w = Tkinter.Canvas(root, width=WIN_X, height=WIN_Y)
w.pack()
rocket = Rocket()

def rotate(x, y, t):
    return x*cos(t) - y*sin(t), x*sin(t) + y*cos(t)

def draw(rocket):
    w.delete("all")
    properties = rocket.draw_info()
    status, heading, gimbal_angle, thrust = properties[0]
    position, goal = properties[1]

    w.create_line(rocket.posX, rocket.posY, rocket.posX + 10 * rocket.velX, rocket.posY)
    w.create_line(rocket.posX, rocket.posY, rocket.posX, rocket.posY + 10 * rocket.velY)
    # draw goal
    w.create_rectangle(goal[0], goal[1], goal[0]-20, goal[1]-5, fill = 'green')
    # draw body
    newX, newY = rotate(rocket.posX, rocket.posY, rocket.heading)
    w.create_rectangle(rocket.posX+5, rocket.posY+5, rocket.posX+15, rocket.posY+35, fill="#fb0")
    # draw engine
    if status:
        w.create_rectangle(rocket.posX+5, rocket.posY, rocket.posX+15, rocket.posY+5, fill="#f50")
    else:
        w.create_rectangle(rocket.posX+5, rocket.posY, rocket.posX+15, rocket.posY+5, fill="#05f")
    w.scale("all", 0.5 * WIN_X, 0.5 * WIN_Y, 1, -1)

def wrap(file, mask):
    data = file.recv(10)
    if len(data) == 0 or data[0] == 'Q':
        root.quit()
    else:
        if rocket.parse(data, file):
            draw(rocket)

draw(rocket)

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind((HOST_ADDR, HOST_PORT))
s.listen(1)

conn, addr = s.accept()
root.tk.createfilehandler(conn, Tkinter.READABLE, wrap)
root .mainloop()
root.tk.deletefilehandler(conn)

print "Program quit"

