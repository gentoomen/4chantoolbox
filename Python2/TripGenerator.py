#!/usr/bin/python2
# TripGenerator.py
#
# Modified from original source by LAMMJohnson
# Licensed under the GPLv3 pending approval from original author, who
# appears to have released it without license (completely restricted)
#
# Source: http://www.mm-rs.org/forums/topic/19070-your-name-in-a-tripcode
# Optimized by The-Soulless
import crypt, getopt, re, signal, string, sys

sys.setcheckinterval(999999)
SIGINT_RECIEVED = False

def errout(msg):
    ''' Display error message, provide usage instructions and exit '''
    print msg, "See", sys.argv[0], "-h for usage instructions"
    exit(0)

def find_trips(regexes, quantity):
    '''
    Loop to continuously try to find @quantity instances of @regexes
    by iterating a string and hashing using mktripcode.
    ret: List of tuples - strings and matching strings they hash to
    '''
    matches = []
    i = 1
    lregexes = regexes
    lquantity = quantity
    while len(matches) < lquantity:
        if SIGINT_RECIEVED:
            return matches
        generated_trip = mktripcode(str(i))
        for regex in lregexes:
            if re.search(regex, generated_trip):
                matches.append((i, "#" + generated_trip))
        i = i + 1
    return matches

def mktripcode(pw):
    ''' Convert a @pw string into a 4chan tripcode hash '''
    pw = pw.decode('utf_8', 'ignore')\
           .encode('shift_jis', 'ignore')\
           .replace('"', '&quot;')\
           .replace("'", '')\
           .replace('<', '&lt;')\
           .replace('>', '&gt;')\
           .replace(',', ',')
    salt = (pw + '...')[1:3]
    salt = re.compile('[^\.-z]').sub('.', salt)
    salt = salt.translate(string.maketrans(':;<=>?@[\\]^_`', 'ABCDEFGabcdef'))
    trip = crypt.crypt(pw, salt)[-10:]
    trip = " " + trip
    return trip

def sigint_handler(sig, _):
    ''' For handling sigint messages gracefully '''
    global SIGINT_RECIEVED
    print "\nSIGINT received."
    SIGINT_RECIEVED = True

def usage():
    ''' Display help/usage text and then exit '''
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

  Hint: You may wish to prepend your searches with (?i) to enable
  case-insensitive matching.
"""
    print "Example Usage:", sys.argv[0], "-r '(?i)gentoo' -n 3"
    exit(0)

def main():
    ''' Main function '''
    short_opts = "hn:r:"
    long_opts = "help number regex".split()
    regexes = []
    quantity = 1

    # Handle sigint correctly
    signal.signal(signal.SIGINT, sigint_handler)

    # Parse arguments
    try:
        opts, args = getopt.gnu_getopt(sys.argv, short_opts, long_opts)
    except:
        errout("Error parsing args.")

    for o, a in opts:
        if o == "-h" or o == "--help":
            usage()
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
    if len(matches) == 0:
        print "No matches found."
    else:
        for num, trip in matches:
            print num, "hashes to", trip
if __name__ == "__main__":
    main()
