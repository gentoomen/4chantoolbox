#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

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

/* Predecs */
void add_image(long l);
void errout(char* str);
short file_exists(char* path);
char* fn_from_url(char* url);
void get_image_links();
void handleargs(int argc, char** argv);
void handle_image_links(void);
short is_match(long l);
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
    FILE *f = fopen(path, "r");

    if (f) {
        fclose(f);
        return 1;
    }

    return 0;
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
        else
            URL = argv[i];
    }
}

void
handle_image_links(void) {
    imageLL *ill;
    char* filename;

    ill = first;
    do {
        filename = fn_from_url(ill->url);

        if (file_exists(filename))
            printf("File exists: %s -- SKIPPING\n", filename);
        else
            printf("PROCESSING LINK: %s as %s\n", ill->url, filename);

        ill = ill->next;
    } while (ill);
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
usage(void) {
    puts("Usage information here.");
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
            handle_image_links();

            curl_easy_cleanup(curl_handle);
        }
        else
            errout("Unable to grab URL.");
    }

    curl_global_cleanup();

    return 0;
}
