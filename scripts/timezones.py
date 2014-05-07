# converts timezones
# python3

timedict = {"PDT":-7, "MDT":-6, "CDT":-5, "EDT":-4, "GMT":0, "BST":1, "CEST":2, "JST":9, "AEST":10}

print("""Timezone Converter: 24h Daylight Savings Time

PDT  | UTC-7  NA Pacific Daylight (Seattle, WA)
MDT  | UTC-6  NA Mountain Daylignt (Denver, CO)
CDT  | UTC-5  NA Central Daylight (Chicago, IL)
EDT  | UTC-4  NA Eastern Daylight (New York, NY)
GMT  | GMT    GMT - Greenwich Mean
BST  | UTC+1  EU British Summer (London, GB)
CEST | UTC+2  EU Central Eastern Summer (Berlin, DE)
JST  | UTC+9  Japan Standard
AEST | UTC+10 AUS Eastern Standard (Sydney, AU)
""")
print("You are in EDT.")
time_1 = str(input("Convert from: "))
if time_1 == "":
    time_1 = "EDT"
    print("EDT selected")
time_2 = str(input("Convert to: "))
if time_2 == "":
    time_2 = "GMT"
    print("GMT selected")
time_3 = [s.strip() for s in str(input("Time: ")).split(":")]
print("---")
hour = (int(time_3[0]) - (int(timedict[time_1]) - int(timedict[time_2])))
if hour >= 24:
    hour -= 24
    print("Advance one day.")
if hour <0:
    hour += 24
    print("Retreat one day.")
print("{} -> {} = {}:{}".format(time_1, time_2, hour, time_3[1]))
