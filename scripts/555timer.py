from math import log

def calc(desr_freq):
    for c in capacitors:
        for r1 in resistors:
            for r2 in resistors:
                resultfreq=1/(c*(r1+2*r2)*log(2))
                dist = abs((desr_freq - resultfreq) / desr_freq)
                #dist = abs((desr_freq - resultfreq))
                if dist < 0.01: # calculate to 1%
                    if 0.25 < 25/r1:
                        continue
                    duty = (r1+r2)/(r1+2*r2)
                    print(str(c*1e6)+" µF",str(r1/1e3)+" kΩ", str(r2/1e3)+" kΩ",str(round(resultfreq,3))+" Hz",\
                      str(round(1/resultfreq,5))+" Sec",\
                      str(round(duty,3)*100)+"%")

capacitors = [1e-12, 5e-12, 6e-12, 10e-12, 15e-12, 20e-12, 22e-12, 30e-12, 33e-12, 47e-12, 68e-12, 100e-12, 220e-12, 330e-12, 470e-12, 680e-12, 1e-9, 2.2e-9, 3.3e-9, 4.7e-9, 10e-9, 22e-9, 33e-9, 47e-9,
              100e-9, 1e-6, 2.2e-6, 3.3e-6, 4.7e-6, 10e-6, 22e-6, 33e-6, 47e-6,
              100e-6, 220e-6, 330e-6, 470e-6]

resistors = [1e3, 1.2e3, 1.5e3, 1.8e3, 2e3, 2.2e3, 3e3, 3.3e3, 3.9e3, 4.7e3,
             5.1e3, 6.8e3, 10e3, 12e3, 15e3, 18e3, 20e3, 22e3, 30e3, 33e3, 39e3,
             47e3, 51e3, 68e3, 100e3, 120e3, 150e3, 180e3, 200e3, 220e3, 300e3,
             330e3, 390e3, 470e3, 510e3, 680e3, 1e6, 2.7e3, 6.2e3, 8.2e3, 27e3,
             62e3, 82e3, 270e3, 620e3, 820e3]

choice = int(input("""Select a parameter to calculate for:
1. Period (s)
2. Frequency (Hz)
> """))

desired = int(input("Enter the value (in seconds or Hz, without units): "))

print("Cap 1","Resist 1", "Resist 2", "Freq", "Period", "Duty Cycle")

if choice == 1:
    print("Calculating for period using", desired, "seconds.")
    calc(1.0/desired)
elif choice == 2:
    print("Calculating for frequency using", desired, "Hz.")
    calc(desired)

print("Cap 1","Resist 1", "Resist 2", "Freq", "Period", "Duty Cycle")
