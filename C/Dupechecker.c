/*
    Finds duplicate files based on md5 and filesize
    Created by LAMMJohnson for the gentoomen 4chantoolbox project
    Licensed under the GPLv3
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
    unsigned char* md5hash;
    unsigned long size;
    char* hrsize;
    fileLL *next, *prev;
};

/* Globals */
unsigned int recursive   = 1;       /* defaults to not recursive */
char*        workdir     = "./";    /* default is current dir */
unsigned int ask         = 1;       /* defaults to requiring confirmation */
unsigned int dummy       = 0;       /* if true, we don't actually delete anything */
unsigned int verbose     = 0;       /* Enables additional output */

fileLL *firstfile = NULL, *lastfile = NULL, *currfile = NULL;

/* Predecs */
void add_file_to_LL(char* path);
void do_delete(fileLL *f);
void errout(char* str);
void free_fileLL(fileLL *f);
char* get_full_path(char* path, char* forf);
unsigned char* get_hash(fileLL *f);
void handleargs(int argc, char** argv);
void handle_match(fileLL *f, fileLL *fc);
int is_dir(char* path);
char* pretty_size(unsigned long in);
void print_hash(unsigned char* str);
void usage(void);
void work_through_dir(char* path);

/* Functions */
void
add_file_to_LL(char* path) {
    fileLL* f;
    struct stat st;

    f = malloc(sizeof(fileLL));
    f->path = malloc((strlen(path) + 1) * sizeof(char));
    strcpy(f->path, path);

    stat(path, &st);
    f->size = st.st_size;
    f->hrsize = pretty_size(f->size);

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
    int match;

    if(!currfile)
        errout("No files to compare!");

    while(currfile) {
        match = 0;
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

                if(!strcmp(currfile->md5hash, fc->md5hash)) {
                    handle_match(currfile, fc);
                    match = 1;
                }
            }
            fc = fc->next;
        }
        if (verbose && !match)
            printf("======== No matches found for file %s\n", currfile->path);

        currfile = currfile->next;
    }

}

void
do_delete(fileLL *f) {
    if (remove(f->path))
        printf("Error in deleting file %s !\n", f->path);
    else
        printf("File %s removed successfully.\n", f->path);

    /* Protection we need when deleting the file we're currently matching against */
    if(currfile == f)
        currfile = currfile->next;

    /* Close the gap in the LL */
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
    free(f->hrsize);
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

unsigned char*
get_hash(fileLL *f) {
    unsigned char *c;
    char *file_buffer;
    int file_descript;
    
    if (verbose)
        printf("======== Getting hash for %s\n", f->path);

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
        else if ( !strcmp("-v", argv[i]) || !strcmp("--verbose", argv[i]) )
            verbose = 1;
        else if( is_dir( argv[i] ) ) {
            workdir = argv[i];
        }
        else {
            printf("Unrecognised command/is not a valid path: %s\n", argv[i]);
            usage();
        }
    }
}

void
handle_match(fileLL *f, fileLL *fc) {
    char c[INLIM];
    int i;

    printf( "=========================== Size Match ===========================\n" );
    printf( "File:    %s\n", f->path );
    printf( "Matches: %s\n", fc->path );
    printf( "Size:    %s\n", f->hrsize );
    print_hash(f->md5hash);

    if (dummy){
        puts("=== Full md5 match found ===");
        return;
    }

    if(ask) {
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
    }
    else
        c[0] = '\0';

    /* Conditions under which we delete the file f. More dangerous because we have to account for moving our pointers */
    if(!strcmp(c, "o") || !strcmp(c, "other") || !strcmp(c, "b") || !strcmp(c, "both" ))
        do_delete(f);

    /* Conditions under which we delete the later file */
    if(!ask || !strcmp(c, "y") || !strcmp(c, "yes") || !strcmp(c, "b") || !strcmp(c, "both" ))
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

char*
pretty_size(unsigned long in) {
    int i = 0;
    double size = in;
    char buf[15], *final;
    const char* units[] = {"B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"};
    while (size > 1024) {
        size /= 1024;
        i++;
    }
    sprintf(buf, "%.*f %s", i, size, units[i]);

    final = malloc((strlen(buf) + 2) * sizeof(char));
    strcpy(final, buf);
    return final;
}

void
print_hash(unsigned char* str) {
    int i;
    printf("Hash is: ");
    for(i = 0; i < MD5_DIGEST_LENGTH; i++) {
        printf("%02x", str[i]);
    }
    printf("\n");
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
    puts(" -v/--verbose      Enable additional output");
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
