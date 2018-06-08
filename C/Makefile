all: Dupechecker.c Scraper.c
	gcc Dupechecker.c -o Dupechecker -lssl -L/usr/lib -lcrypto
	gcc Scraper.c -o Scraper -lcurl -g -Wall
	chmod +x Dupechecker Scraper

clean:
	rm Dupechecker Scraper
