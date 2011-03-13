/*
Finds duplicate files based on md5 and filesize
Created by LAMMJohnson for the gentoomen 4chantoolbox project
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/dir.h>

typedef struct fileLL fileLL;
struct fileLL {
    char* path;
    char* md5hash;
    fileLL *next;
};

/* Globals */
unsigned int recursive   = 1;       /* defaults to not recursive */
char*        workdir     = "./";    /* default is current dir */
unsigned int ask         = 1;       /* defaults to requiring confirmation */
unsigned int dummy       = 0;       /* if true, we don't actually delete anything */

fileLL *firstfile = NULL, *lastfile = NULL;

void add_file_to_LL(char* path);
void errout(char* str);
char* get_full_path(char* path, char* forf);
void handleargs(int argc, char** argv);
int is_dir(char* path);
void usage(void);
void work_through_dir(char* path);

void
add_file_to_LL(char* path) {
    fileLL* f = malloc(sizeof(fileLL));

    f->path = malloc((strlen(path) + 1) * sizeof(char));
    strcpy(f->path, path);

    if (!firstfile) {
        firstfile = f;
        lastfile = firstfile;
    }
    else {
        lastfile->next = f;
        lastfile = lastfile->next;
    }
}

void
errout(char* str) {
    if (str)
        puts(str);
    exit(1);
}

char*
get_full_path(char* path, char* forf) {
    char* c = malloc( (strlen(path) + strlen(forf) + 2) * sizeof(char) );
    c[0] = '\0';
    strcat(c, path);
    strcat(c, forf);
    if (is_dir(c))
        strcat(c, "/");
    return c;
}

void
handleargs(int argc, char** argv) {
    int i;
    for (i = 1; i < argc; i++) {
        if ( !strcmp("-d", argv[i]) || !strcmp("--dummy", argv[i]) )
            dummy = 1;
        else if ( !strcmp("-r", argv[i]) || !strcmp("--recursive", argv[i]) )
            recursive = 1;
        else if ( !strcmp("-y", argv[i]) || !strcmp("--yes-to-all", argv[i]) )
            ask = 0;
        else if ( !strcmp("-h", argv[i]) || !strcmp("--help", argv[i]) )
            usage();
        else if( is_dir( argv[i] ) ) {
            puts("DING!");
            workdir = argv[i];
        }
        else
            printf("Unrecognised command/is not a valid path: %s\nContinuing anyway.\n", argv[i]);
    }
}

int
is_dir(char* path) {
    DIR *dir = opendir(path);
    int ret = 0;

    if (dir)
        ret = 1;

    closedir(dir);
    return ret;
}

void
usage(void) {
    puts("==================================================================");
    puts("Usage: Dupechecker-Python [OPTION] <directory>");
    puts("Script to remove duplicate files");
    puts(" ");
    puts(" -d/--dummy        No deletion. Output findings without prompting");
    puts(" -r/--recursive    Recurse through directories");
    puts(" -y/--yes-to-all   Remove duplicates without prompting");
    puts(" -h/--help         This help text");
    puts("==================================================================");
    exit(1);
}

void
work_through_dir(char* path) {
    struct dirent *ent;
    DIR* dir = opendir(path);
    char* fullpath;

    while((ent = readdir(dir))){
        /* Skip over them if we're getting ".." or "." from readdir() */
        if(!strcmp("..", ent->d_name) || !strcmp(".", ent->d_name))
            continue;

        fullpath = get_full_path(path, ent->d_name);

        if(is_dir(fullpath)) {
            if(recursive)
                work_through_dir(fullpath); /* Recursively start working on a subdir */
        }
        /* Otherwise it's just a normal file and we need to work our magic */
        else
            add_file_to_LL(fullpath);

        free(fullpath);
    }
    closedir(dir);
}

int main(int argc, char** argv) {

    handleargs(argc, argv);

    if (!is_dir(workdir))
        errout("Failed to open given directory.");

    work_through_dir(workdir);
    
    fileLL *f;
    for(f = firstfile; f; f = f->next)
        printf("FILE: %s\n", f->path);


    return 0;
}
/*
class fileblock():
    def __init__(self, inpath):
        self.wholepath = inpath
        self.size = os.path.getsize(inpath)
        self.md5 = ""

    def get_md5(self):
        print "Generating md5 for " + self.wholepath
        self.md5 = md5_for_file(self.wholepath)

def add_dir(dirpath):
    print "Scanning directory " + dirpath
    for tempfile in os.listdir(dirpath):
        if os.path.isdir(dirpath + tempfile) and recursive:
            add_dir(dirpath + tempfile + "/")
        elif os.path.isfile(dirpath + tempfile):
            filestats.append( fileblock( dirpath + tempfile ) )

# get user input to determine if it's deleteion time or not
def get_choice():
    choose = ""
    validOptions = ( 'y', 'n', 'o', 'b', 'yes', 'no', 'other', 'both')
    while not choose in validOptions :
        choose = raw_input('(y)es / (n)o / the (o)ther file / (b)oth ? ')
    return choose

def md5_for_file(filename, block_size=2**20):
    f = open(filename)
    md5 = hashlib.md5()
    while True:
        data = f.read(block_size)
        if not data:
            break
        md5.update(data)
    return md5.digest()

def pretty_size(size):
    suffixes = [("B",2**10), ("K",2**20), ("M",2**30), ("G",2**40), ("T",2**50)]
    for suf, lim in suffixes:
        if size > lim:
            continue
        else:
            return round(size/float(lim/2**10),2).__str__()+suf

def remfile(infile):
    if os.path.isfile(infile.wholepath):
        os.remove(infile.wholepath)
        print infile.wholepath + " deleted."

def showmatch(filea, fileb):
    print "=========================== Size Match ==========================="
    print "File:    " + filea.wholepath
    print "Matches: " + fileb.wholepath
    print "Size:    " + pretty_size( filea.size )

def usage():
    print "=================================================================="
    print "Usage: Dupechecker-Python [OPTION] <directory>"
    print "Script to remove duplicate files"
    print " "
    print " -d/--dummy        No deletion. Output findings without prompting"
    print " -r/--recursive    Recurse through directories"
    print " -y/--yes-to-all   Remove duplicates without prompting"
    print " -h/--help         This help text"
    print "=================================================================="
    sys.exit(' ')

## Rip out any genuine paths in the args
for arg in sys.argv:
    if os.path.isdir(arg):
        directory = arg

## Handle Args
try:
    opts, args = getopt.gnu_getopt( sys.argv, "dhry", ["dummy", "help", "recursive", "yes-to-all"] )

except getopt.GetoptError:
    print str(err)
    usage()

for o, a in opts:
    if o in ("-d", "--dummy"):
        dummy = True
    elif o in ("-r", "--recursive"):
        recursive = True
    elif o in ("-y", "--yes-to-all"):
        ask = False
    elif o in ("-h", "--help"):
        usage()

# Spit out a warnign if we're using the current dir
if directory == "./":
    print "No valid directory given. Using current directory."

# If the dir doesn't end with a "/" we need to add one
if not directory[ len(directory) - 1 ] == "/":
    directory = directory + "/"

## Main part
#Get a list of files only (no dirs)
add_dir(directory)

#to allow us to catch exceptions
# The real meat of it -- compare listed files.
for i in range(len(filestats)):
    startfile = filestats[i]
    # be careful to check we haven't deleted either the file in a previous action
    if os.path.isfile(startfile.wholepath):

        for e in range(i + 1, len(filestats)):
            checkagainst = filestats[e]
            # be careful to check we haven't deleted either the file in a previous action
            if os.path.isfile(checkagainst.wholepath):

                if startfile.size == checkagainst.size:
                    showmatch(startfile, checkagainst)

                    # Generate md5s if necessary
                    for f in (startfile, checkagainst):
                        if f.md5 == "":
                            f.get_md5()
                
                    if startfile.md5 == checkagainst.md5:
                        if dummy:
                            print "=== Full md5 match found ==="

                        else:
                            print "Match found! Would you like to delete duplicate file " + startfile.wholepath + ""
    
                            # Allowance for the automatic overwrite flag
                            if ask:
                                choice = get_choice()

                            # In instructed to, delete the file    
                            if choice in ('o', 'other', 'b', 'both'):
                                remfile(checkagainst)
                            if choice in ('y', 'yes', 'b', 'both') or not ask:
                                remfile(startfile)
                                break
                    else:
                        print "No md5 match."
*/
