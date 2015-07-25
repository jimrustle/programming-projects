
from random import randint
from math import sin, cos, pi

DELTA_T = 0.1

class Rocket:
    def __init__(self):
        self.posX, self.posY = randint(0, 100), 0
        self.goalX, self.goalY = randint(200, 550), randint(0, 500)
        self.engine_status = False
        self.engine_gimbal = pi/2
        self.heading = 0
        self.thrust = 0
        self.velY = 0
        self.velX = 0
        self.accelX = 0
        self.accelY = 0

    def parse(self, in_data, conn):
        tokens = in_data.replace('\n','').split(' ')
        first_char = tokens[0]
        draw_status = False
        if first_char == 'I':
            conn.send(self.info())
        elif first_char == 'E':
            if len(tokens) < 2:
                print "Error: 'E' command needs another argument"
            else:
                if tokens[1] == "ON":
                    self.engine_status = True
                else:
                    self.engine_status = False
        elif first_char == 'H':
            if len(tokens) < 2:
                print "Error: 'H' command needs another argument"
            else:
                new_angle = float(tokens[1])
                if new_angle < -10:
                    new_angle = -10
                elif new_angle > 10:
                    new_angle = 10
                else:
                    self.engine_gimbal = new_angle * pi/180.0 + pi/2
                    print self.engine_gimbal
        elif first_char == 'T':
            if len(tokens) < 2:
                print "Error: 'T' command needs another argument"
            else:
                new_thrust = float(tokens[1])
                if new_thrust < 0:
                    self.thrust = 0
                elif new_thrust > 100:
                    self.thrust = 100
                else:
                    self.thrust = new_thrust
        elif first_char == 'S':
            self.step()
            draw_status = True
        else:
            print "Error: unknown command {}".format(in_data)

        return draw_status

    def info(self):
        return ",".join(str(val) for val in self.draw_info()) + "\n"

    def draw_info(self):
        return ([self.engine_status, self.heading, self.engine_gimbal, self.thrust],
                [(self.posX, self.posY), (self.goalX, self.goalY)])

    def step(self):
        # update accelerations
        thrust = 30 * self.thrust/100.0 * self.engine_status
        self.accelX = thrust * cos(self.engine_gimbal)
        self.accelY = (thrust * sin(self.engine_gimbal) - 9.8)
        print self.accelX, self.accelY
        # update velocities
        self.velX += self.accelX * DELTA_T
        self.velY += self.accelY * DELTA_T
        # update positions
        self.posX += self.velX * DELTA_T
        self.posY += self.velY * DELTA_T
        if self.posY < 0:
            self.posY = 0
            self.velY = 0
            self.velX = 0
