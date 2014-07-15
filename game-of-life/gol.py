def shiftup(array):
    k = list(array)
    old = k[:7]
    return k[7:]+old

def shiftdown(array):
    k = list(array)
    old = k[-7:]
    return old + k[:42]

def shiftleft(array):
    k = list(array)
    old = k[:49:7]
    for i in range(7):
        for j in range(6):
            k[7*i+j] = k[7*i+j+1]
        k[7*i+6] = old[i]
    return k

def shiftright(array):
    k = list(array)
    old = k[6:49:7]
    for i in range(6, -1, -1):
        for j in range(6, -1, -1):
            k[7*i+j] = k[7*i+j-1]
        k[7*i] = old[i]
    return k

def add(a, b, c, d, e, f, g, h):
    k = [0]*49
    for i in range(49):
        k[i] = a[i] + b[i] + c[i] + d[i] \
               + e[i] + f[i] + g[i] + h[i]
    return k

lifearray = []

for i in range(1, 8):
    for j in range(7):
        lifearray.append(0)

lifearray[5] = 1
lifearray[12]= 1
lifearray[19] = 1

#lifearray[9] = 1
#lifearray[10] = 1
#lifearray[16] = 1
#lifearray[17] = 1

def next_state(current):
    k = add(shiftup(current), \
                       shiftdown(current), \
                       shiftleft(current), \
                       shiftright(current), \
                       shiftup(shiftleft(current)), \
                       shiftup(shiftright(current)), \
                       shiftdown(shiftleft(current)), \
                       shiftdown(shiftright(current)))
    return [1 if ((current[i] == 1 and k[i] == 2) or k[i] == 3) else 0 for i in range(49)]

def print_array(a):
    for i in range(7):
        for j in range(7):
            print a[7*i+j],
        print
    print "---"

print_array(lifearray)
print_array(next_state(lifearray))
print_array(next_state(next_state(lifearray)))
