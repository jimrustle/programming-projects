#!/usr/bin/python3
# 03 September 2013
# Jim Rustle <jim_rustle@live.com>
#
# python3 script to convert a selection of utf-8 characters into ascii
# characters suitable for reading on legacy devices that do not support utf-8
#
# an example would be the Playstation Portable running the Bookr software
#
from sys import argv

def main():
    punctuation_dict = {'“':'"', '”':'"', '‘':"\'", '’':"\'", '…':'...',\
            '—':'-', '–':'-', ' ':' ', 'é':'e', 'ï':'i'}

    if len(argv) == 1:
        return 1
    else:
        for i in range(1,len(argv)):
            with open(argv[i], 'r') as oldfile, \
            open((argv[i]).replace(".txt","-f.txt"), 'w') as newfile:
                for line in oldfile:
                    for char in punctuation_dict:
                        line = line.replace(char, punctuation_dict[char])
                    newfile.write(line)
                newfile.write("\n\n\n")
        return 0

if __name__ == "__main__":
    if main():
        # returns 1 upon failure
        print("Error: No text file arguments supplied!\n\
Usage: python3 change_punct.py text1.txt ... textN.txt")
    else:
        # returns 0 on success
        print("Job completed!")
