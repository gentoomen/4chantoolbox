#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

/* Globals */
CURL *curl;
CURLcode *res;

/* Predecs */
void errout(char* str);
void handleargs(int argc, char** argv);
void usage(void);

void
errout(char* str) {
    if(str)
        puts(str);
    exit(0);
}

void
handleargs(int argc, char** argv) {
    int i;
    for (i = 0; i < argc; i++) {
        if (!strcmp(argv[i], "-h"))
            usage();
    }
}

void
usage(void) {
    puts("Usage information here.");
    exit(0);
}

int main(int argc, char** argv) {
    curl = curl_easy_init();

    if(argc < 2) {
        puts("Not enough arguments given.");
        usage();
    }
    else
        handleargs(argc, argv);

    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, argv[1]);

        printf("Grabbing URL: %s\n", argv[1]);

    }

    return 0;
}
