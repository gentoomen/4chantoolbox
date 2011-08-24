# 4chan Toolbox
## A toolkit for maximum trolling.

So we made this thing to help people learn programming languages and since
we're very close to 4chan's technology board (/g/) we decided that this should
contain stuff related to 4chan and/or /g/.

## Guidelines:

The structure of this repo should stay like this:

- a directory with the name of a programming language (e.g. `python/`, `ruby/`, ...)
  - a bunch of scripts (see next section)
  - a custom `LICENCE` file (optional - we're using GPLv3 by default on all projects
    (read the NOTE inside `LICENCE`))
- and `README.md` and `LICENCE`

The language-folders should contain the approved scripts, which are (at the moment):

- the Dupechecker - which checks for duplicate files in a folder by calculating MD5
hashes and stuff and,
- the Scraper - which downloads images from any 4chan thread 

Those scripts should work pretty much the same way in every language - the main idea
is, that when somebody already knows one language it might be easier to learn the
other one by looking at the exact source in the two different languages.

Pretty much anyone can contribute - just run a pull request and if it fits with
the 4chantoolbox-guidelines it'll get merged in.

## Other notes:

\>mfw the old, blank README file was the only commit by agaric left in this repo :/
