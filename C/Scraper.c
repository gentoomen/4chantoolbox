/*
    Written by LAMMJohnson
    For the 4chantoolbox project
    Licensed under the GPLv3
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>
#include <sys/dir.h>

/* Datatypes */
struct MemoryStruct {
  char          *memory;
  size_t        size;
};

typedef struct imageLL imageLL;
struct imageLL {
    char        *url;
    imageLL     *next;
};

/* Globals */
CURL                    *curl_handle;
CURLcode                res;
char                    *URL;
struct MemoryStruct     URLdata;
imageLL                 *first = NULL, *curr;
unsigned int            downtot;

/* Default Dettings */
short                   verbose = 1;
char*                   outdir = "./";
short                   refresh = 10;

/* Predecs */
void add_image(long l);
void errout(char* str);
short file_exists(char* path);
char* fn_from_url(char* url);
void free_imageLL(imageLL* ill);
void get_image_links();
void handleargs(int argc, char** argv);
void handle_image_links(void);
short is_dir(char* path);
short is_match(long l);
void retrieve_file(char* url, char* path);
void sanitise_outdir(void);
void usage(void);
static size_t write_memory_callback(void *ptr, size_t size, size_t nmemb, void *data);

/* Functions */
void
add_image(long l) {
    imageLL *ill = malloc(sizeof(imageLL));
    int sz;

    if(!first)
        curr = first = ill;

    for(sz = 0; URLdata.memory[sz + l] != '"'; sz++);
    ill->url = malloc((sz + 1) * sizeof(char));
    strncpy(ill->url, URLdata.memory + (l * sizeof(char)), sz);

    ill->next = NULL;
    curr->next = ill;
    curr = ill;
}

void
errout(char* str) {
    if(str)
        puts(str);
    exit(0);
}

short
file_exists(char* path) {
    FILE *f;
    char *fullpath;
    short ret = 0;

    fullpath = malloc((strlen(outdir) + strlen(path)) * sizeof(char));
    strcpy(fullpath, outdir);
    strcat(fullpath, path);

    f = fopen(fullpath, "r");
    if (f) {
        fclose(f);
        ret = 1;
    }

    free(fullpath);
    return ret;
}

char*
fn_from_url(char* url) {
    char* name;
    int i, len;

    for (i = len = strlen(url); url[i] != '/'; i--);

    name = malloc((len - i) * sizeof(char));
    strncpy(name, url + i + 1, len - i);

    return name;
}

void
free_imageLL(imageLL* ill) {
    free(ill->url);
    free(ill);
}

void
get_image_links(void) {
    long l;
    int linksgrabbed = 0;       /* This is a horrible hack. Each image is listed twice so we only take every other one. */

    for (l = 0; l < URLdata.size; l++) {
        if (URLdata.memory[l] == 'h' && is_match(l)) {
            /* Hackish. Horrible. */
            if(linksgrabbed % 2)
                add_image(l);
            linksgrabbed++;
        }
    }
}

void
handleargs(int argc, char** argv) {
    int i;
    for (i = 0; i < argc; i++) {
        if (!strcmp(argv[i], "-h"))
            usage();
        else if (!strcmp(argv[i], "-o") || !strcmp(argv[i], "--output")) {
            if (++i < argc)
                outdir = argv[i];
        }
        else
            URL = argv[i];
    }

}

void
handle_image_links(void) {
    imageLL *ill, *ill_old;
    char* filename;

    ill = first;
    do {
        filename = fn_from_url(ill->url);

        if (file_exists(filename))
            printf("File exists: %s -- SKIPPING\n", filename);
        else
            retrieve_file(ill->url, filename);

        ill_old = ill;
        ill = ill->next;
        free(filename);
        free_imageLL(ill_old);
    } while (ill);
}

short
is_dir(char* path) {
    DIR *dir = opendir(path);
    int ret = 0;

    if (dir)
        ret = 1;

    closedir(dir);
    return ret;
}

short
is_match(long l) {
    char* strtomatch = "http://images.4chan.org/";
    int i, lim = strlen(strtomatch);

    for (i = 0; i < lim; i++)
        if (URLdata.memory[i + l] != strtomatch[i])
            return 0;

    return 1;
}

void
retrieve_file(char* url, char* path) {
    CURL *fcurl;
    FILE *f;
    char *fullpath;

    printf("Downloading file: %s\n", path);

    fcurl = curl_easy_init();
    if (fcurl) {
        fullpath = malloc((strlen(outdir) + strlen(path) + 1) * sizeof(char));
        strcpy(fullpath, outdir);
        strcat(fullpath, path);

        f = fopen(fullpath, "w");
        curl_easy_setopt(fcurl, CURLOPT_URL, url);
        curl_easy_setopt(fcurl, CURLOPT_WRITEDATA, f); 
        curl_easy_perform(fcurl);
        curl_easy_cleanup(fcurl);
        fclose(f);

        free(fullpath);

        printf("%s downloaded successfully.\n", path);
    }
    else
        printf("Error with libcurl downloading file: %s\nTo Path: %s\n", url, path);

}

void
sanitise_outdir(void) {
    int len = strlen(outdir);
    char* s;

    if(outdir[len - 1] == '/')
        return;

    s = malloc((len + 2) * sizeof(char));
    strcpy(s, outdir);
    strcat(s, "/");
    /* free(outdir); */
    outdir = s;
}

void
usage(void) {
    puts("==================================================");
    puts("Usage: Scraper [OPTION] <thread url>");
    puts("Default OPTIONS: --output ./ --thread 10");
    puts("Scraper for 4chan, part of 4chantoolbox.");
    puts("");
    puts(" -o/--output set output dir");
    puts(" -q/--quiet go silent");
    puts(" -t/--timer N thread refresh timer");
    puts(" -h/--help this message");
    puts("==================================================");
    exit(0);
}

static size_t
write_memory_callback(void *ptr, size_t size, size_t nmemb, void *data) {
  size_t realsize = size * nmemb;
  struct MemoryStruct *mem = (struct MemoryStruct *)data;
 
  mem->memory = realloc(mem->memory, mem->size + realsize + 1);
  if (mem->memory == NULL) {
    /* out of memory! */ 
    printf("not enough memory (realloc returned NULL)\n");
    exit(EXIT_FAILURE);
  }
 
  memcpy(&(mem->memory[mem->size]), ptr, realsize);
  mem->size += realsize;
  mem->memory[mem->size] = 0;
 
  return realsize;
}

int
main(int argc, char** argv)
{
    curl_handle = curl_easy_init();

    if(argc < 2) {
        puts("Not enough arguments given.");
        usage();
    }
    else
        handleargs(argc, argv);

    sanitise_outdir();
    if(!is_dir(outdir)) {
        printf("Invalid directory for output (dir does not exist): %s", outdir);
        errout(NULL);
    }


    if(curl_handle) {
        curl_easy_setopt(curl_handle, CURLOPT_URL, URL);
        curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, write_memory_callback);
        curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void*) &URLdata);
        curl_easy_setopt(curl_handle, CURLOPT_USERAGENT, "Firefox/3.6.15");

        curl_easy_perform(curl_handle);

        if(URLdata.memory) {
            printf("Grabbed URL: %s\n", URL);
            /* printf("Data:\n%s", URLdata.memory); */

            get_image_links();
            free(URLdata.memory);
            curl_easy_cleanup(curl_handle);

            handle_image_links();
        }
        else
            errout("Unable to grab URL.");
    }

    curl_global_cleanup();

    return 0;
}
