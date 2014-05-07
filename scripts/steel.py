#
# Calculates loss in coins of steel bar -> steel nail production
#

# 621819 gp
# 668 gp
# 44 gp

from math import floor

def nails():
    money = input("Money to spend: ")
    steelbar = input("Cost per steel bar: ")
    steelnail = input("Cost per steel nail: ")
    print("-----")

    purchase_amt = floor(float(money)/steelbar)
    print "Purchase: {0} steel bars".format(purchase_amt)
    sell = purchase_amt*15*steelnail
    purchase_prc = purchase_amt*steelbar
    print "Remainder: {0}".format(money-purchase_prc)
    total=money-purchase_prc+sell
    print "Total: {0}, net profit = {1}".format(total, sell-purchase_prc)

def superheat():
    money = input("Money to spend: ")
    coal = 4*input("Price of coal: ")
    mithril = input("Price of mithril ore: ")
    nature = input("Price of nature rune: ")
    mbar = input("Price of mithril bar: ")

    purchase_amt = floor(money/(coal+mithril+nature))

    print ("Buy {0} mithril ores, {0} nature runes, and {1} coal.").format(purchase_amt, 4*purchase_amt)
    print ("Net: {0}").format(mbar*purchase_amt-money)

superheat()
#nails()
