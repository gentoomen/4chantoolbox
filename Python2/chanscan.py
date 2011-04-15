"""A simple procedural-style web-scanner that reads through the text in
threads on 4chan.org to find user-specified keywords.

Created by: renjikken (ryuurei)
Contact me: irc.rizon.net
Created on: April 14th 2011
Version no: 0.1

The program works by doing the following:
1. Generating http://boards.4chan.org/[board]/ -> ".../[board]/15"
2. Scanning each of those pages for threads depicted by the text "res/d+"
   where d+ is a series of numbers specifying the thread number.
3. Reading through each line of the source of each page and searching for
   each keyword within each line.

To run the program, enter:
python chanscan.py [board] [keywords,separated,by,commas]
into your command prompt/terminal.
For example, if you wanted to find "programming" and "hacking" in technology,
you would enter text similar to the following:
python chanscan.py /g/ programming,hacking
"""

import urllib
import sys

BASE_URL = "http://boards.4chan.org"

# The user needs to specify their arguments (the board name and list of
# keywords) via the command line.
if len(sys.argv) != 3:
    print("Run this program as {0} [board] [keywords]".format(sys.argv[0]))
    print("eg: {0} /b/ mudkipz,lolcats".format(sys.argv[0]))
    sys.exit(1)
    
board = sys.argv[1]
keywords = sys.argv[2].split(",")
pages = [BASE_URL + board]
threads = []
passed = []
matches = []

# Generate each of the pages to scan. These are
# http://boards.4chan.org/board/ to http://boards.4chan.org/board/15
for i in xrange(1, 16):
    pages.append(BASE_URL + board + str(i))
# Scan through each of the pages looking for occurrences of the text
# "res/" followed by a sequence of numbers denoting the thread id.
for page in pages:
    print("Scanning page: {0}".format(page))
    try:
        page_data = urllib.urlopen(page)
    except IOError:
        break
    for line in page_data:
        if "res/" in line:
            temp = "res/"
            # If a link exists in the line, read the numbers from the position
            # of "res/" in the string + 4 onwards. Add four to skip over
            # the text "res/". This is done to skip to the thread id numbers.
            for letter in line[line.index("res/")+4:]:
                if letter in "1234567890":
                    temp += letter
                else:
                    break
            # Check to make sure we haven't already been over the thread so
            # that we don't waste computation power going over threads we've
            # already passed (printing takes more time than iterating).
            if not temp in threads:
                threads.append(temp)
                print("Found thread {0}".format(temp))
# Go through each thread and read their text, looking for each user-specified
# keyword in the line until one is found, then break out of the thread's page
# as it is not necessary to find more than one keyword in any given page.
for thread in threads:
    url = BASE_URL + board + thread
    print("Reading {0}".format(url))
    found = False
    try:
        page = urllib.urlopen(url)
    except IOError:
        print("Could not open {0}".format(url))
        break
    for line in urllib.urlopen(url):
        for keyword in keywords:
            if keyword in line and not thread in passed:
                print("Found {0} in {1}".format(keyword, thread))
                passed.append(thread)
                found = True
                break
        if found:
            found = False
            break
# Cycle through the list of passed thread IDs to print out a single list of
# URLs that the user can follow to get to the threads within which one or 
# more of the keywords they entered were found within.
print("\nList of threads matching some of your keywords: ")
for item in passed:
    print(BASE_URL + board + item)
    