/*
Finds duplicate files based on md5 and filesize
Created by LAMMJohnson for the gentoomen 4chantoolbox project
*/

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/dir.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include <openssl/md5.h>

#define INLIM 6

/* Structs */
typedef struct fileLL fileLL;
struct fileLL {
    char* path;
    char* md5hash;
    unsigned int size;
    fileLL *next, *prev;
};

/* Globals */
unsigned int recursive   = 1;       /* defaults to not recursive */
char*        workdir     = "./";    /* default is current dir */
unsigned int ask         = 1;       /* defaults to requiring confirmation */
unsigned int dummy       = 0;       /* if true, we don't actually delete anything */

fileLL *firstfile = NULL, *lastfile = NULL, *currfile = NULL;

void add_file_to_LL(char* path);
void do_delete(fileLL *f);
void errout(char* str);
void free_fileLL(fileLL *f);
char* get_full_path(char* path, char* forf);
char* get_hash(fileLL *f);
void handleargs(int argc, char** argv);
void handle_match(fileLL *f, fileLL *fc);
int is_dir(char* path);
void show_match(fileLL *a, fileLL *b);
void usage(void);
void work_through_dir(char* path);

void
add_file_to_LL(char* path) {
    fileLL* f;
    struct stat st;

    f = malloc(sizeof(fileLL));
    f->path = malloc((strlen(path) + 1) * sizeof(char));
    strcpy(f->path, path);

    stat(path, &st);
    f->size = st.st_size;

    f->md5hash = NULL;

    if (!firstfile) {
        firstfile = f;
        lastfile = firstfile;
        firstfile->prev = NULL;
    }
    else {
        lastfile->next = f;
        f->prev = lastfile;
        lastfile = f;
    }

    lastfile->next = NULL;
}

void
compare_files(void) {
    fileLL *fc;
    currfile = firstfile;

    if(!currfile)
        errout("No files to compare!");

    while(currfile) {
        fc = currfile->next;
        while(fc) {
            if(currfile->size == fc->size) {
                /* At this point we're iterating through the linked list comparing every file to every other file. */
                /* If we're in this loop we've found files of matching size. */
                /* Now we need to fill in hashes if we don't already have them */
                if(!currfile->md5hash)
                    currfile->md5hash = get_hash(currfile);
                if(!fc->md5hash)
                    fc->md5hash = get_hash(fc);

                show_match(currfile, fc);
                if(!strcmp(currfile->md5hash, fc->md5hash))
                    handle_match(currfile, fc);
                else
                    puts("But hashes do not match.");
            }
            fc = fc->next;
        }
        currfile = currfile->next;
    }

}

void
do_delete(fileLL *f) {
    if (remove(f->path) == -1)
        printf("Error in deleting file %s !\n", f->path);
    else
        printf("File %s removed successfully.\n", f->path);

    /* Protection we need when deleting the file we're currently matching against */
    if(currfile == f)
        currfile = currfile->next;

    if (f->prev)
        f->prev->next = f->next;
    if (f->next)
        f->next->prev = f->prev;

    free_fileLL(f);
}

void
errout(char* str) {
    if (str)
        puts(str);
    exit(1);
}

void
free_fileLL(fileLL *f) {
    free(f->path);
    free(f->md5hash);
    free(f);
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

char*
get_hash(fileLL *f) {
    char *c, *file_buffer;
    int file_descript;
    
    c = malloc((MD5_DIGEST_LENGTH + 1) * sizeof(char));
    c[MD5_DIGEST_LENGTH] = '\0';

    file_descript = open(f->path, O_RDONLY);
    if(file_descript < 0) exit(-1);
    file_buffer = mmap(0, f->size, PROT_READ, MAP_SHARED, file_descript, 0);

    MD5((unsigned char*) file_buffer, f->size, c);

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
            workdir = argv[i];
        }
        else
            printf("Unrecognised command/is not a valid path: %s\nContinuing anyway.\n", argv[i]);
    }
}

void
handle_match(fileLL *f, fileLL *fc) {
    char c[INLIM];
    int i;

    if (dummy){
        puts("=== Full md5 match found ===");
        return;
    }

    printf( "MD5 match found! Would you like to delete duplicate file %s ?\n", fc->path );

    /* Sanitary method of getting the user's choice */
    do {
        puts("(y)es / (n)o / the (o)ther file / (b)oth ?");
        for (i = 0; i < INLIM && (c[i] = getchar()) != '\n'; i++);
        if (c[i] != '\n')
            while(getchar() != '\n');
        c[i] = '\0';
    }
    while (strcmp(c, "o") && strcmp(c, "other") && strcmp(c, "b") && strcmp(c, "both") && strcmp(c, "y") && strcmp(c, "yes") && strcmp(c, "n") && strcmp(c, "no"));

    /* Conditions under which we delete the file f. More dangerous because we have to account for moving our pointers */
    if(!strcmp(c, "o") || !strcmp(c, "other") || !strcmp(c, "b") || !strcmp(c, "both" ))
        do_delete(f);

    /* Conditions under which we delete the later file */
    if(!strcmp(c, "y") || !strcmp(c, "yes") || !strcmp(c, "b") || !strcmp(c, "both" ))
        do_delete(fc);
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
show_match(fileLL *a, fileLL *b) {
    printf( "=========================== Size Match ===========================\n" );
    printf( "File:    %s\n", a->path );
    printf( "Matches: %s\n", b->path );
    printf( "Size:    %d\n", a->size );
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

    /* Spit out a warning if we're using the current dir */
    if (!strcmp(workdir, "./"))
        puts("No valid directory given. Using current directory.");

    work_through_dir(workdir);
    
    compare_files();

    puts("Checking finished. Exiting successfully.");

    return 0;
}
/*
# get user input to determine if it's deleteion time or not
def get_choice():
    choose = ""
    validOptions = ( 'y', 'n', 'o', 'b', 'yes', 'no', 'other', 'both')
    while not choose in validOptions :
    return choose

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
                
                    else:
                        print "No md5 match."
*/
