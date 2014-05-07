#!/usr/bin/python3
# 03 September 2013
# Jim Rustle <jim_rustle@live.com>
#
# python3 script to split a file with chapter headers into separate files
#
import sys
def split(oldtext, filename):
    chapternumber = 0
    chapter = ["CHAPTER","Chapter"]
    newfile =  open(("{0}-Chapter_{1}.txt".format(filename\
            .replace(".txt",""), chapternumber)), 'a')
    for line in oldtext:
        # if 'Chapter' or 'CHAPTER' appears in the first word of a line of
        # text, then close the old file and start a new one with a new name
        if len(line.split()) != 0 and line.split()[0] in chapter:
            newfile.write("\n\n\n")
            newfile.close()
            chapternumber += 1
            newfile = open(("{0}-Chapter_{1}.txt".format(filename\
                    .replace(".txt",""), chapternumber)), "a")
            newfile.write(line)
        else:
            newfile.write(line)
    newfile.write("\n\n\n")
    newfile.close()

def main():
    # no arguments
    if len(sys.argv) == 1:
        return 0
    else:
        for i in range(1,len(sys.argv)):
            master_text = open(sys.argv[i], 'r')
            split(master_text, sys.argv[i])
            master_text.close()
        return 1

if __name__ == "__main__":
    if main():
        print("Job completed!")
    else:
        print("Error: No text file arguments supplied!\nUsage: python3\
split_chapters.py text1.txt ... textN.txt")
