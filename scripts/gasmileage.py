# converts gasoline mileage

def convertUnits(fuel, quantity):
    fueldict={"MPG US":0.425143707, "MPG UK":0.354006189, "KM/L":1}
    fuellist=["MPG US", "MPG UK", "KM/L"]
    if fuel in fuellist:
        print "%f %s" % (quantity*fueldict[fuel]/fueldict["MPG UK"], "MPG UK")
        print "%f %s" % (quantity*fueldict[fuel]/fueldict["MPG US"], "MPG US")
        print "%f %s" % (quantity*fueldict[fuel], "km/l")
        print "%f %s" % (100/(quantity*fueldict[fuel]), "l/100km")
    else:
        print "%f %s" % (100/(quantity*fueldict["MPG US"]), "MPG US")
        print "%f %s" % (100/(quantity*fueldict["MPG UK"]), "MPG UK")
        print "%f %s" % (100/(quantity*fueldict["km/l"]), "km/l")
        print "%f %s" % (quantity, "l/100km")
    return None

print """Fuel Economy Converter
Available units:
MPG US
MPG UK
km/l
l/100km
"""

fuel = raw_input("Fuel? (example: km/l, MPG US) \n>>> ")
quantity = input("Quantity? (example: 100, 24, 56) \n>>> ")

convertUnits(fuel.upper(), quantity)
