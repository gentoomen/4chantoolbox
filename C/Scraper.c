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
void get_image_links();
void handleargs(int argc, char** argv);
void handle_image_links(void);
void usage(void);
static size_t write_memory_callback(void *ptr, size_t size, size_t nmemb, void *data);

/* Functions */
void
add_image(long l) {
    imageLL *ill = malloc(sizeof(imageLL));

    if(!first) {
        curr = first = ill;
    }

    ill->url = "TESTTESTTEST";
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

void
get_image_links(void) {
    long l;

    for (l = 0; l < URLdata.size; l++) {
        if (URLdata.memory[l] == 'h'){
            add_image(l);
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

    ill = first;
    do {
        printf("PROCESSING LINK: %s\n", ill->url);

        ill = ill->next;
    } while (ill);
}

void
usage(void) {
    puts("Usage information here.");
    exit(0);
}

static size_t
write_memory_callback(void *ptr, size_t size, size_t nmemb, void *data)
{
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

int main(int argc, char** argv) {
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
            handle_image_links();

            curl_easy_cleanup(curl_handle);
        }
        else
            errout("Unable to grab URL.");
    }

    curl_global_cleanup();

    return 0;
}
