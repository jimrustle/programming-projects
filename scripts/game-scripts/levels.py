from autopy import key
from time import sleep
import random

L = "L"
R = "R"
U = "U"
D = "D"

def focus():
    print "You have five seconds to focus on the window"
    sleep(5)

def rand_del():
    return random.random()/10

def wait_bomb():
    sleep(1 + rand_del())

def end_level():
    key.tap(' ')
    sleep(0.5)
    key.tap(' ')
    sleep(0.5)
    key.tap(' ')
    sleep(2)

def m(dir, n = 1, delay = 0.10):
    delay += random.random()/10 # adds random 0 to 0.1 delay
    if dir == "L":
        for i in range(n):
            key.tap(key.K_LEFT)
            sleep(delay)
    elif dir == "R":
        for i in range(n):
            key.tap(key.K_RIGHT)
            sleep(delay)
    elif dir == "U":
        for i in range(n):
            key.tap(key.K_UP)
            sleep(delay)
    elif dir == "D":
        for i in range(n):
            key.tap(key.K_DOWN)
            sleep(delay)

def parse(level_string):
    for token in level_string.split(" "):
        if len(token) == 1:
            m(token)
        elif ',' in token:
            s = token.split(",")
            m(token[0], int(s[1:]), float(s[1]))
        elif len(token) == 2:
            m(token[0], int(token[1:]))
        elif token == "wait_bomb":
            wait_bomb()
        elif token == "end_level":
            end_level()
        else:
            print("fuggg broken token", token)

lv1 = "R7 D7 R2 U10 R wait_bomb R U L2 D L4 U L8 R D7 L U R wait_bomb D3 L D2 R9 U2 L4 U R4 U2 L R wait_bomb L4 R4 U2 R3 D3 L2 U L7 end_level"

lv2 = "L U2 R5 D2 U2 L U2 R6 D4 L3 R3 D2 L D2 R D U wait_bomb D2 L14 U2 R2 L2 D2 R14,0.05 U2 L3 R wait_bomb L8,0.05 R10,0.05 U6 L2 R2 D8 L14,0.05 U2 R U4 L R U4 R7 D2 L4 D2 R4 end_level"

lv3 = "L D9 L11,0.05 U2 R U6 L D4 R D2 L D2 R11,0.05 U2 R3 D U wait_bomb D2 L3 U10,0.05 R3 D6 L U5 L U L8 R wait_bomb L D2 L D6 R6 U4 L4 D2 R4 D2 L6 U8 L3 end_level"

lv4 = " R U D R R U D R R U D R R U D R R U R wait_bomb R2 D3 U3 L3 D R2 wait_bomb R D2 L L2 D L U L2 D L U L2 D L U L D L3 U2 D2 R2 D2 R11,0.05 L2 wait_bomb R2 D2 L13,0.05 D2 R12,0.05 L11,0.05 U2 R U4 L2 U5,0.05 D5,0.05 R2 D4 R11,0.05 L2 wait_bomb R2 D2 R end_level"

lv5 = "U D3 L2 U2 L2 U4,0.05 R6,0.05 L2,0.25 L4,0.05 D9,0.05 L3 R3 U R4 U6,0.05 R2 D2 R end_level"

lv6 = "L4 D5 U R9 U2 R5 U4 L U2 D2 L7,0.05 U L3 D L3 U2 D2 R14,0.05 D7 U L6 D L D U L U R3 U2 R4 U R U L5 R wait_bomb L4 end_level"

lv7 = "L D R L wait_bomb R4 U R L D L4 U D wait_bomb U4 R L U D wait_bomb U3 R U L U3 R5 L2 wait_bomb R2 D2 L2 D3 U3 R2 D R D2 R3 D2 R D L4 wait_bomb R3 D2 L D R3 U5 R U6 R2 end_level"

lv8 = "R R R3,0.5 U6 L D2 R D2 L3 U5 L9,0.05 D11,0.05 R6 L wait_bomb R2 U2 D2 R4 L wait_bomb R4 L10,0.05 U4 R L D4 L4 U4 R3 L3,0.01 U U6 R5 D5 L3 R3 U5 R4 D5 R5 U2 D2 L5 U5 L2 D5 U5 L7,0.05 end_level"

lv9 = "L7 D6 U8 R U3 D3 L D2 R6 U2 R U D wait_bomb U2 R4 U L8 D R4 D2 L4 D6 L D2 L R U6 R U R8 D R3 U2 L U3 wait_bomb R D5 L2 D2 L5 R5 D4 R L U3 L5 D U9 end_level"
