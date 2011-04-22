import urllib
import HTMLParser
import threading
import optparse
import sys
import re

usage = """A simple web-scanner that reads through the text in\n
threads on 4chan.org to find user-specified keywords.\n
\n
Created by: renjikken (ryuurei)\n
Contact me: irc.rizon.net\n
Created on: April 14th 2011\n
Version no: 0.2\n
\n
run this program with `python {0} <options> <arguements>`\n
Options include:\n
>> Specifications <<\n
These two pieces of information must both be given a value.\n
-b / --board  [board to scan] i.e. /b/, /g/, /sci/, etc. <MANDATORY>\n
-k / --keys   [comma,separated,list,of,keywords/phrases]  <MANDATORY>\n
\n
>> Key Data <<\n
Specify what it is you want to match your keys to. Defaults to -p \n
-p / --inpost [0 or 1 for no or yes] Match users' comments.\n
-n / --inname [0 or 1 for no or yes] Match user names.\n
-t / --intrip [0 or 1 for no or yes] Match user tripcodes.\n
-e / --inmail [0 or 1 for no or yes] Match user emails.\n
\n
>> Miscelanious <<\n
Specify other options for functionality modification.\n
-f / --first  [0 < int < 16] specifies page to start at. Default = 0\n
-l / --last   [0 < int < from] specifies page to stop scan at. Default = 16\n
-c / --count  [0 or 1 for no or yes] Print count rather than matches.\n
""".format(sys.argv[0])

BASE_URL = "http://boards.4chan.org"
SCANPOST = "-p" # Try to match keywords in content of posts.
SCANNAME = "-n" # Try to match keywords in names of posters.
SCANTRIP = "-t" # Try to match keywords in tripcodes of posters.
SCANMAIL = "-e" # Try to match keywords in emails of posters.
OUTCOUNT = "-c" # Ouput only the number of matches for each phrase.
OUTCANDM = "-m" # Output both the number of matches and the containing text.
print_lock = threading.Lock()

class Scanner(threading.Thread):
    def __init__(self, url, board, keywords, switches):
        threading.Thread.__init__(self)
        self.url = url
        self.keywords = keywords
        self.cmd = switches
        self.board = board

    def run(self):
        list_threads = find_threads(self.url)
        # Dictionary of {keyword : list of text containing keyword}
        matches = {}
        for key in self.keywords:
            matches[key] = []
        if list_threads is None:
            print("Could not access {0}.\nQuitting.".format(self.url))
            sys.exit(1)
        for thread in list_threads:
            url = BASE_URL + self.board + thread
            try: page_data = urllib.urlopen(url).read()
            except IOError:
                print("Could not read {0}".format(url))
                continue
            post_text = ""
            for line in page_data:
                for key in self.keywords:
                    if SCANNAME in self.cmd and "class=\"postername\"" in line:
                        name = re.match("\>.+\<", line).group(0)
                        name = name.replace(">", "").replace("<", "")
                        if key in name:
                            matches[key].append(name)
                    if SCANTRIP in self.cmd and "class=\"postertrip\"" in line:
                        trip = re.match("\>.+\<", line).group(0)
                        trip = trip.replace(">", "").replace("<", "")
                        if key in trip:
                            matches[key].append(trip)
                    if SCANMAIL in self.cmd and "href=\"mailto:" in line:
                        mail = re.match("mailto:.+\"", line).group(0)
                        mail = mail.replace("mailto:", "").replace("\"", "")
                        if key in mail:
                            mathces[key].append(mail)
                if SCANPOST in self.cmd and "<blockquote>" in line:
                    next_line = page_data.readline()
                    temp_text = ""
                    if "class=\"unkfunc\"" in next_line:
                        temp_text = next_line[next_line.rindex(";")+1:]
                        if "</font>" in temp_text:
                            temp_text = temp_text.replace("</font>", "")
                        post_text = "\n".join([post_text, temp_text])
                    elif "</font>" in next_line:
                        temp_text = next_line.replace("</font>", "")
                        post_text = "\n".join([post_text, temp_text])
                    elif (">" not in line) and ("<" not in line):
                        post_text = "\n".join([post_text, line])
                if "</blockquote>" in line and post_text != "":
                    for key in self.keywords:
                        if key in post_text:
                            matches[key].append(post_text)
                    post_text = ""
            for key in matches.keys():
                if OUTCANDM in self.cmd:
                    print("{0} results for {1} in {2}.".format(
                        len(matches[key]),
                        key,
                        thread))
                    for i in matches[key]:
                        print(i)
                    print("")
                elif OUTCOUNT in self.cmd:
                    print("{0} results for {1} in {2}.".format(
                        len(matches[key]),
                        key,
                        thread))
                    print("")
                else:
                    print("Matches for {0}:".format(key))
                    for i in matches[key]:
                        print(i)
                    print("")

def remove_duplicates(list_item):
    return list(set(list_item))
    
def find_threads(url):
    try: 
        page_data = urllib.urlopen(url).read()
    except IOError: 
        return None
    return remove_duplicates(re.findall("res/\d+", page_data))

def main():
    parser = optparse.OptionParser(usage)
    parser.add_option(
        "-b",
        "--board",
        dest="board",
        type="str",
        help="The board to scan. (eg: /g/, /b/, /c/, /jp/, /sci/ etc.)")
    parser.add_option(
        "-k",
        "--keys",
        dest="keys",
        type="str",
        help="A comma-separated list of keywords/phrases to use in search")
    parser.add_option(
        "-f",
        "--first",
        dest="from_page",
        type="int",
        default=0,
        help="The first page to begin scanning at")
    parser.add_option(
        "-l",
        "--last",
        dest="to_page",
        type="int",
        default=15,
        help="The last page to stop scanning at")
    parser.add_option(
        "-p",
        "--inpost",
        dest="scan_posts",
        type="int",
        default=1,
        help="Match keywords in users' comments.")
    parser.add_option(
        "-n",
        "--inname",
        dest="scan_names",
        type="int",
        default=0,
        help="Match keywords in users' names.")
    parser.add_option(
        "-t",
        "--intrip",
        dest="scan_trips",
        type="int",
        default=0,
        help="Match keywords in users' tripcodes.")
    parser.add_option(
        "-e",
        "--inmail",
        dest="scan_mails",
        type="int",
        default=0,
        help="Match keywords in users' emails.")
    parser.add_option(
        "-c",
        "--count",
        dest="out_count",
        type="int",
        default=0,
        help="Output count of matches rather than the actual matching text")
    parser.add_option(
        "-m",
        "--candm",
        dest="match_both",
        type="int",
        default=0,
        help="Output both the count of matches and containing text.")
    for arguement in sys.argv:
        if "-h" in arguement or "--help" in arguement:
            print("python {0} <options> <arguements>".format(sys.argv[0]))
            print("Options for this program include:")
            print("-b")
    option, args = parser.parse_args()
    if not option.board:
        print("A board must be specifed with -b (eg: /b/).")
        sys.exit(1)
    if not option.keys:
        print("Comma separated keywords must be specified with -k.")
        sys.exit(1)
    board = option.board
    keywords = option.keys.split(",")
    if option.from_page == 0:
        pages = [BASE_URL + board]
    elif option.from_page > 0 and option.from_page < 16:
        if option.to_page >= option.from_page and option.to_page <= 16:
            pages = []
            for i in xrange(option.from_page, option.to_page):
                pages.append(BASE_URL + board + str(i))
        else:
            print("Page to finish scan must be greater than the page set to ")
            print("start the scan at.")
            sys.exit(1)
    else:
        print("Page specified to start at must be > 0 and < 16")
        sys.exit(1)
    switches = ""
    if option.scan_posts != 0:
        switches += SCANPOST
    if option.scan_names != 0:
        switches += SCANNAME
    if option.scan_trips != 0:
        switches += SCANTRIP
    if option.scan_mails != 0:
        switches += SCANMAIL
    if option.out_count != 0:
        switches += OUTCOUNT
    if option.match_both != 0:
        switches += OUTCANDM
    for i in pages:
        Scanner(i, board, keywords, switches).start()

if __name__ == "__main__":
    main()