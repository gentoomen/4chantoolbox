all: Dupechecker.c Scraper.c
	gcc Dupechecker.c -o Dupechecker -lssl
	gcc Scraper.c -o Scraper -lcurl -g
	chmod +x Dupechecker Scraper

clean:
	rm Dupechecker Scraper
