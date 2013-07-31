#!/usr/bin/python2
# TripGenerator.py
#
# Modified from original source by LAMMJohnson
# Licensed under the GPLv3 pending approval from original author, who
# appears to have released it without license (completely restricted)
#
# Source: http://www.mm-rs.org/forums/topic/19070-your-name-in-a-tripcode

import re, string, crypt, sys, time, getopt

def errout(msg):
    print msg, "See", sys.argv[0], "-h for usage instructions"
    exit(0)

def find_trips(regexes, quantity):
    matches = []
    i = 1
    while len(matches) < quantity:
        generated_trip = mktripcode(str(i))
        if i % 100000 == 0:
            print "Generated", i, "tripcodes..."
        for regex in regexes:
            if re.search(regex, generated_trip):
                print "FOUND tripcode:", i, "=>", generated_trip
                matches.append((i, "#" + generated_trip))
        i = i + 1
    return matches

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

def usage():
    print sys.argv[0], "is a tripcode generation program"
    print """
You must provide at least one regex to match against, and
this program will find a number of tripcodes that match
that regex for you.

Flags:
  -h or --help
      Print this help text and exit
  -n <#> or --number <#>     :: (Default 1)
      Number of matches to find before exiting
  -r <str> or --regex <str>
      Provide regex to match against. You may provide multiple
      regexes and they will all be matched against. You MUST provide
      at least one regex to match against
"""

def main():
    short_opts = "hn:r:"
    long_opts = "help number regex".split()
    regexes = []
    quantity = 1

    # Parse arguments
    try:
        opts, args = getopt.gnu_getopt(sys.argv, short_opts, long_opts)
    except:
        errout("Error parsing args.")

    for o, a in opts:
        if o == "-h" or o == "--help":
            usage()
            return 0
        elif o == "-n" or o == "--number":
            quantity = int(a)
        elif o == "-r" or o == "--regex":
            regexes.append(a)
        else:
            errout("Unrecognised argument " + a)

    # Handle incorrect options
    if regexes == []:
        errout("No regexes to match supplied.")
    if quantity < 1:
        errout("Error :: Instructed to find less than 1 match")

    # Report work to be done to user
    print "Finding", quantity, "matches of:"
    for s in regexes:
        print s

    # Generate matches and report findings
    matches = find_trips(regexes, quantity)
    for num, trip in matches:
        print num, "hashes to", trip

if __name__ == "__main__":
    main()
