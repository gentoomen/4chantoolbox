#!/usr/bin/python2
# TripGenerator.py
# Writes the # and it's tripcode to a file
#
# Source: http://www.mm-rs.org/forums/topic/19070-your-name-in-a-tripcode
#

import re, string, crypt, sys, time

## SETTINGS
TRIPS = 2 # number of trips to generate

def mktripcode(pw):
    pw = pw.decode('utf_8', 'ignore') \
            .encode('shift_jis', 'ignore') \
            .replace('"', '&quot;') \
            .replace("'", '')\
            .replace('<', '&lt;')\
            .replace('>', '&gt;')\
            .replace(',', ',')
    salt = (pw + '...')[1:3]
    salt = re.compile('[^\.-z]').sub('.', salt)
    salt = salt.translate(string.maketrans(':;<=>?@[\\]^_`', 'ABCDEFGabcdef'))
    trip = crypt.crypt(pw, salt)[-10:]
    return trip

def finder(trp):
    global TRIPS
    x,y,i = 0,0,1
    total_trips = {}
    start = time.time()
    while i <= TRIPS:
        if re.search(trp, str.lower(mktripcode(str(x))))>-1:
            print "#FOUND tripcode %d" % i
            total_trips['#'+ str(x)] = mktripcode(str(x))
            i = i+1
        elif x==y:
            print "Generating.." + str(x)
            y=y+100000
        x=x+1
    return total_trips


username = sys.argv[1]
carry_on = True

if len(username) > 4:
    choice = raw_input("this can take a long time, are you sure? (y/n): ")
    if (choice in ('y','Y','yes','YES','Yes')):
        carry_on = True
    else:
        carry_on = False

if carry_on:
    data = finder(username)
    print "\nAll done:\n"
    for k, v in data.iteritems():
        print(k + ': ' + v)
print "\nuse the #num as password in the name field to use the tripcode.\nEnjoy!\n"