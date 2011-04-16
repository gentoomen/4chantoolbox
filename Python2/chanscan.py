"""A simple web-scanner that reads through the text in
threads on 4chan.org to find user-specified keywords.

Created by: renjikken (ryuurei)
Contact me: irc.rizon.net
Created on: April 14th 2011
Version no: 0.2
"""

import urllib
import sys
import re

BASE_URL = "http://boards.4chan.org"

def remove_duplicates(list_item):
    """Converts a list to a set then back to a list to remove duplicates."""
    return list(set(list_item))

if len(sys.argv) != 3:
    print("Run this program as {0} [board] [keywords]".format(sys.argv[0]))
    print("eg: {0} /b/ mudkipz,lolcats".format(sys.argv[0]))
    sys.exit(1)
    
board = sys.argv[1]
keywords = sys.argv[2].split(",")
pages = [BASE_URL + board]

# Create a list of the pages from .../[board]/ to .../[board]/15
for i in xrange(1, 16):
    pages.append(BASE_URL + board + str(i))
for page in pages:
    try: page_data = urllib.urlopen(page).read()
    except IOError: continue
    print("Scanning page: {0}".format(page))
    for i in remove_duplicates(re.findall("res/\d+", page_data)):
        url = BASE_URL + board + i
        try: thread_source = urllib.urlopen(url).read()
        except IOError:
            print("Could not open {0}".format(url))
            break
        for keyword in keywords:
            if re.search(keyword, thread_source) is not None:
                print("Found \"{0}\" in {1}".format(keyword, url))
                break