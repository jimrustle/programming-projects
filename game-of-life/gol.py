SIZE = 20

def shiftup(array):
    k = list(array)
    old = k[:SIZE]
    return k[SIZE:]+old

def shiftdown(array):
    k = list(array)
    old = k[-SIZE:]
    return old + k[:SIZE*(SIZE-1)]

def shiftleft(array):
    k = [0]*SIZE**2
    old = array[0:SIZE**2:SIZE]
    for i in range(0, SIZE**2 - 1):
            k[i] = array[i+1]
    for i in range(SIZE-1, SIZE**2, SIZE):
        k[i] = old[i/SIZE]
    return k

def shiftright(array):
    k = [0]*SIZE**2
    old = array[SIZE-1:SIZE**2:SIZE]
    for i in range(1, SIZE**2):
            k[i] = array[i-1]
    for i in range(0, SIZE**2, SIZE):
        k[i] = old[i/SIZE]
    return k

def add(a, b, c, d, e, f, g, h):
    k = [0]*SIZE**2
    for i in range(SIZE**2):
        k[i] = a[i] + b[i] + c[i] + d[i] \
               + e[i] + f[i] + g[i] + h[i]
    return k

def next_state(current):
    k = add(shiftup(current), \
                       shiftdown(current), \
                       shiftleft(current), \
                       shiftright(current), \
                       shiftup(shiftleft(current)), \
                       shiftup(shiftright(current)), \
                       shiftdown(shiftleft(current)), \
                       shiftdown(shiftright(current)))
    return [1 if ((current[i] == 1 and k[i] == 2) or k[i] == 3) else 0 for i in range(SIZE**2)]

def print_array(a):
    for i in range(SIZE):
        for j in range(SIZE):
            print a[SIZE*i+j],
        print
    print "---"

lifearray = []

for i in range(SIZE):
    for j in range(SIZE):
        lifearray.append(0)

lifearray[5] = 1
lifearray[25]= 1
lifearray[45] = 1

#lifearray[9] = 1
#lifearray[10] = 1
#lifearray[16] = 1
#lifearray[17] = 1


print_array(lifearray)
print_array(next_state(lifearray))
print_array(next_state(next_state(lifearray)))
