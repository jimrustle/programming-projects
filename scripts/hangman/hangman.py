from random import randint
#from string import upper, translate
import sys
import os

chances = 0
letters = []
guesses = []
wrong_guess = []
wordlist = ["CLERIHEW", "SPORADIC", "INVARIABLY", "APOPHASIS", "CATACHRESIS",
            "ONOMATOPOEIA", "ANACOLUTHON", "RESPITE", "PARALIPSIS",
            "PSEUDOLOGY", "POTVALIANCY", "FASTIGIATE", "EXORDIUM", "GASCONADE",
            "SOLECISM", "PROFLIGACY", "PERSNICKETY", "HETEROTELIC", "IDONEOUS",
            "REMORA", "MULTIPLICATION", "INCARNADINE", "REDOLENT", "INNOCUOUS",
            "CONVIVIAL", "TENEBROUS", "AESTHETE", "TRISKAIDEKAPHOBIA",
            "EXCULPATE", "MILIEU", "SYCOPHANT", "EXCOGITATE", "AUSCULTATION",
            "ADAMANTINE", "PERORATION"]

def intro():
    global chances, wrong_guess, word, letters, guesses
    chances = 6
    letters = []
    guesses = []
    wrong_guess = []
    word = wordlist[randint(0, len(wordlist) - 1)]
    for letter in word:
        guesses.append(0)
        letters.append(letter)
    print "Let's play (Yet Another) Hangman!"
    print "Your word has", len(letters), "letters."
    raw_input("Press enter to continue")

def play():
    global chances
    while True:
        guess()
        if chances == 0:
            print "You lose! The word was", word
            break
        if guesses.count(1) == len(guesses):
            print "You win! The word was", word
            break
    choice = raw_input("Play again?")
    if choice.upper() == "Y" or choice.upper() == "YES":
        return True
    else:
        return False

def guess():
    global chances, guesses, letters, wrong_guess
    os.system("clear")
    print "You have", chances, "tries available."

    # print a blank for a missing letter, or print the letter if you've
    # guessed it
    for i, num in enumerate(guesses):
        if num == 0:
            sys.stdout.write("_")
        else:
            sys.stdout.write(letters[i])

    # print the letters you've guessed
    print "\n"
    print "Wrong letters:"
    for i in wrong_guess:
        print i,

    # prompts for a letter, and if it's a letter you've not already
    # guessed, either allow the choice and display the word
    # or add the letter to the wrong letters list
    print "\n"
    choice = raw_input("Please pick a letter. \n>")
    for i, letter in enumerate(letters):
        if choice.upper() == letter:
            guesses[i] = 1
    if choice.upper() not in letters and choice.upper() not in wrong_guess:
        wrong_guess.append(choice.upper())
        chances -= 1

os.system("clear")

pl = True
while pl:
    intro()
    pl = play()
