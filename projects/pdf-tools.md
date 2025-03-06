---
title: PDF bibliography tools
---

## Intro ##

Every time I find a document online (mostly PDFs, but postscript, sometime HTML, etc.) I save it to my `Documents` directory. This directory now contains many files, on many diverse topics; from type theory, to particle Physics, to AI. Whenever I recall reading some particular fact, I can usually find the reference in that directory (usually via a simple `grep`).

The major problem with this approach is that it's quite tedious to actually cite any of these files, since they don't have associated bibliographic information. I keep a "master" BibTeX file called `Documents/ArchivedPapers/Bibtex.bib`, which I add entries to whenever the need arises, in the following way:

 - Open the relevant document
 - Enter some of its details (title, author, etc.) into a search engine like [Google Scholar](http://scholar.google.com)
 - Copying the most likely-looking BibTeX entry
 - Paste into `Bibtex.bib`
 - Move that document file into `ArchivedPapers`
 - Add a `localfile` key pointing to the document file

In fact, some of this is made a little smoother by [KBibTeX](http://home.gna.org/kbibtex/), which combines a BibTeX editor, document viewer and search engine into one tool. KBibTeX is certainly nice to use as a *viewer* of the documents which are already in `Bibtex.bib`, but unfortunately it's still sort of clunky to do the above kind of import procedure, since it neccessarily involves viewing documents which aren't in the database yet. It certainly makes a decent effort, with [Dolphin](https://www.kde.org/applications/system/dolphin/) and [Okular](https://okular.kde.org/) built in, but requires an awful lot of context-switching between the different "panes"/tabs.

Recently I decided to *automatically* import as many of these documents as possible, to see how far I could get. This document describes the various approaches I've taken, as well as providing handy commandline snippets which I can use in the future.

## Document Properties ##

Each document can be considered to have a bunch of *properties*, which can influence how easy or hard it is to import it automatically. Here are some I've come across:

 - Filetype: I'm only considering PDFs for now, since postscript, HTML, etc. are few enough for me to import manually.
 - Scanned: PDFs of old documents, eg. many from the 1960s and earlier, will be scans; essentially, one giant image. This is difficult to handle, since it doesn't contain any machine-readable strings of text. Some documents may be converted via OCR (optical character recognition), although there may be mis-spellings, etc. in their results.
 - Metadata: PDFs can contain metadata, like author and title, in a similar way to MP3s and JPEGs. If available, this can be extracted very easily.
 - DOIs: a digital object identifier (DOI) is a form of URI which uniquely identifies a document. If a document contains its DOI on the first couple of pages, it can be extracted easily.

## Approaches ##

Some of these may work for you straight away, some may require tweaking, some may prove hopeless. I'd give each a try, and move on if you have too many difficulties.

### Zotero ###

[Zotero](https://www.zotero.org/) is a bibliography manager, built around Mozilla's XUL toolkit. Making it work on [NixOS](http://nixos.org/) is [a little tricky](https://github.com/NixOS/nixpkgs/issues/10355).

Zotero has a nice workflow for importing PDF files:

 - Create a database
 - Add to it links to the PDF files we wish to import
 - Select those links
 - Choose "Retrieve PDF metadata"
 - Export the resulting BibTeX and copy into your real BibTeX file

This will extract metadata from the PDFs, search for it online (eg. using Google Scholar) and present any BibTeX it finds. There are two major problems with this approach:

 - If there is no metadata to extract, it usually fails (it tries the filename, but this may be unhelpful)
 - There seems to be a request limit for Google Scholar. Even after filling in some CAPTCHAs, I couldn't get it to work for more than a couple of dozen files.

### Docear ###

[Docear](http://www.docear.org/) is a rather bloated application for managing "projects", which just-so-happen to contain bibliographies. Its reference management is built on [JabRef](http://jabref.sourceforge.net/), but seems to work better in my experience. Similar to Zotero, this can work well for getting the "low hanging fruit", like PDFs with existing metadata.

I've made a quick and dirty [Nix package for Docear](/git/warbo-dotfiles/git/branches/master/nixpkgs/local/docear.nix).

### Scholar.py ###

[scholar.py](https://github.com/ckreibich/scholar.py) is a script for querying Google Scholar from the commandline. Whilst not the most useful thing in the world on its own, it's great for embedding into scripts. One thing to keep in mind when calling `scholar.py` is that it will crash if given non-ascii characters, so you should run your strings through `iconv` first to transliterate them.

I've made a little [Nix package for scholar.py](/git/warbo-dotfiles/git/branches/master/nixpkgs/local/scholar.nix).

### pdfmeat ###

[pdfmeat](https://code.google.com/p/pdfmeat/) is a Python script which tries to extract data from a PDF.

I've made a little [Nix package for pdfmeat](/git/warbo-dotfiles/git/branches/master/nixpkgs/local/pdfmeat.nix), although you'll have to download the source yourself since Google Code was giving me inconsistent hashes. I've also packaged its dependencies [translitcodec](/git/warbo-dotfiles/git/branches/master/nixpkgs/local/translitcodec.nix) and [subdist](/git/warbo-dotfiles/git/branches/master/nixpkgs/local/subdist.nix).

### pdfssa4met ###

This [bizarrely named tool](https://code.google.com/p/pdfssa4met) is yet another Python script for handling PDF files. Once again, I've [packaged it for Nix](/git/warbo-dotfiles/git/branches/master/nixpkgs/local/pdfssa4met.nix).

### searchtobibtex ###

[searchtobibtex](https://github.com/atisharma/searchtobibtex) is a collection of handy scripts for extracting information from PDF files. Be warned: it includes tools to destructively rename PDF files based on its results. I tend to avoid this, and instead just get the metadata printed out, which I can act on in a subsequent "phase".

Here's my [Nix package for searchtobibtex](/git/warbo-dotfiles/git/branches/master/nixpkgs/local/searchtobibtex.nix), as well as one for [bibclean](/git/warbo-dotfiles/git/branches/master/nixpkgs/local/bibclean.nix) which it depends on.

### pdf-extract ###

I came across [pdf-extract](https://github.com/CrossRef/pdfextract) during my travels, although haven't tried it (or at least, I don't appear to have made a Nix package for it).

## Processes ##

These are roughly the steps I followed to import as many PDFs as possible. It hasn't caught everything, but it's saved me a *lot* of time compared to the purely manual approach.

### Low Hanging Fruit ###

Send everything through Zotero and Docear, see what sticks. I don't trust these tools with my real BibTeX database (it's in git, but I don't like wading through massive diffs), so I tend to import into a fresh database, then copy/paste the result over to the real one.

### Embedded DOIs ###

TODO: This should give easy wins, but I couldn't extract a single DOI from my documents for some reason.

### Extractable Titles ###

For those tricky PDFs which don't contain metadata or DOIs, but *do* contain machine-readable text, I've found the following process to be useful (note, I've had to reconstruct some commands from memory; they may need tweaking):

Loop through all files, echoing out the file name and the output of `headings.py --title` applied to the file. `headings.py` is part of pdfssa4met, and will output titles in XML. Something like this:

```bash
for FILE in Documents/*.pdf
do
  T=$(headings.py --title "$FILE")
  [[ -z "$T" ]] && continue
  echo "<file><name>$FILE</name>$T</file>"
done | tee TITLES
```

This will make a file `TITLES` associating PDF filenames with their titles (if found).

We can use `xidel` to loop through these, and send each title to `searchtobibtex` to find a reasonable looking BibTeX entry from [CrossRef](http://www.crossref.org/). We then echo out the file name, the extracted title and any BibTeX we found, with a bunch of sentinel strings sprinkled in (`FILE:`, `TITLE:`, `BIB:` and `ENDBIB`).

```bash
# This XPath3 expression will produce tab-separated pairs of titles (with newlines stripped out) and filenames
xidel - --extract-kind=xpath3 \
        --extract "//*[text()='$F']/../(pdf/title/replace(text(), '\n', '') || '	' || name)" < TITLES |
while read -r LINE
do
  # Use cut to extract the elements of each pair
  TITLE=$(echo "$LINE" | cut -f 1)
  FILE=$(echo "$LINE" | cut -f 2)
  # Look up some BibTeX for this title
  BIB=$(searchtobibtex "$TITLE")
  echo -e "FILE: $FILE\nTITLE: $TITLE\nBIB:\n$BIB\nENDBIB"
done | tee BIBOUT
```

Make a copy of the output (eg. `BIBOUT2`), and process it with Emacs macros:

 - Remove any FILE and TITLE lines which aren't followed by a BibTeX entry
 - Remove all BibTeX content except for the `title` key. Use a different sentinel (eg. `GOT`) for these online titles, to disambiguate from the TITLE we extracted.
 - Remove all newlines from BibTeX titles
 - Make all titles (extracted and BibTeX) lowercase (`C-x C-l`)
 - Remove all non-alphabetic letters from all titles (`replace-regexp`)
 - Put each `FILE`/`TITLE`/`GOT` into one line, separated by tabs, eg. `FILE	foo.pdf	TITLE	atitle	GOT	atitle`

Loop through these lines in a shell, echoing out those filenames where the (normalised) extracted title matches the (normalised) BibTeX title. It's pretty likely that these have been correctly extracted and looked up:

```bash
cat BIBOUT2 | while read -r LINE
do
  FILE=$(echo  "$LINE" | cut -f 2)
  TITLE=$(echo "$LINE" | cut -f 4)
  GOT=$(echo   "$LINE" | cut -f 6)
  [[ "x$TITLE" = "x$GOT" ]] && echo "$FILE"
done
```

In Emacs again, make another copy of the extracted BibTeX results (eg. `BIBOUT3`) and use a macro to:

 - Search for `ENDBIB`
 - Find the preceding `FILE:` sentinel
 - Copy the filename
 - Go forward to the `ENDBIB`
 - Append a `localfile = "..."` entry to the BibTeX

Next, remove all `FILE`, `TITLE`, `BIB` and `ENDBIB` lines, just leaving the raw BibTeX.

Using an Emacs macro, do the following:

 - Switch to a buffer of filenames which had matching extracted/searched titles
 - Cut (kill, copy then remove, whatever) the first one
 - Switch to the BibTeX entries annotated with `localfile` keys
 - Go to the start of the buffer, then search for `localfile = "<PASTE FILENAME HERE>"`
 - Copy the surrounding BibTeX entry
 - Switch to a BibTeX file (either your main one, or a temporary file)
 - Go to the end of the buffer and paste the BibTeX entry

Repeat this until you've got entries for all of the PDFs which had successfully extracted titles. I prefer to invoke this macro manually each time, rather than specifying some number of repetitions, so that I can give each BibTeX entry a quick glance for credibility (eg. Does it even look like BibTeX? Does the title sound familiar? Does it look like the kind of document I would have saved?)

### Titles in File Names ###

For those which weren't caught above, try this rather manual process. List all filenames twice, with this format:

```
FILE	foo.pdf	TITLE	foo
```

Manually remove lines which don't look like they'll be useful search terms, ie. their name probably doesn't resemble their title (eg. `002.pdf` is probably not worth searching for). For the rest, perform a bunch of regex-replaces on the `TITLE` parts:

 - Replace punctuation with spaces
 - Prefix capital letters with a space (eg. `FooBar` becomes `Foo Bar`)
 - Search for `[A-Z] [A-Z]` and fix broken initialisms (eg. turn `A S T` back into `AST`)

Run the results through `searchtobibtex`:

```bash
cat TITLES | while read -r LINE
do
  FILE=$(echo "$LINE" | cut -f 2)
  TITLE=$(echo "$LINE" | cut -f 4)
  BIB=$(searchtobibtex "$TITLE")
  echo -e "FILE: $FILE\nTITLE: $TITLE\nBIB\n$BIB\nENDBIB"
done
```

Inspect the results to remove those which are clearly unrelated, then use a macro to add `localfile` keys containing the file names. Remove the sentinel values, and append to the main BibTeX file as above.

### Manual Title Entry ###

This is pretty horrible, so it's a bit of a last resort. Loop through all file names and wrap them in a BibTeX entry containing a `localfile` field. In my case, I used the key `zzzz$MD5` where `$MD5` is the file's MD5 hash.

With this in place, we can use a little Emacs Lisp to loop through every entry, and for each one we open the PDF, querying for the title and insert it into the BibTeX entry.

```commonlisp
(defun take-name ()
  ;; Find the next localfile key which doesn't have a title
  (re-search-forward "^@misc{zzzzz.*,[\n][\t]localfile = \"[^\"]*\"[\n]")
  (forward-line -1)
  (beginning-of-line)
  (re-search-forward "localfile = ")
  ;; Copy the contents
  (forward-char)  ;; Past "
  (set-mark (point))
  (end-of-line)
  (backward-char) ;; Past "
  ;; Open the file in a temporary doc-view buffer
  (let ((selection (buffer-substring-no-properties (mark) (point)))
        (title     ""))
    (with-temp-buffer
      (insert-file-contents selection)
      (doc-view-mode)
      (switch-to-buffer (current-buffer))
      (sit-for 2)
      (doc-view-fit-width-to-window)
      ;; Query for the title
      (setq title (read-from-minibuffer "Title: ")))
    ;; Insert a title key into the BibTeX
    (end-of-line)
    (insert ",\n\ttitle = \"")
    (insert title)
    (insert "\"")))
```

This at least saves us from having to click around GUIs to do our data entry. At the end we will have a bunch of BibTeX entries with `title` and `localfile` fields.

### Expanding Meagre Entries ###

If all you have is a file and a title, you can look it up on Google Scholar to get more info. Note that this will give up after a few dozen, probably due to rate limiting; just try again after a while:

```bash
# Grep for the pattern which we used for our keys, getting the line numbers
grep -n "@misc{zzzz" ArchivedPapers/Bibtex.bib |
cut -d ':' -f 1 |
while read -r NUM
do
  # Extract the BibTeX entry beginning at each line number (should be 4 lines: the opening/closing braces, the localfile and the title)
  CTX=$(( NUM + 3 ))
  ENTRY=$(head -n"$CTX" ArchivedPapers/Bibtex.bib | tail -n4)

  # Get the filename and title fields
  FILE=$(echo  "$ENTRY" | grep localfile | sed -e 's/.*localfile = "\(.*\)".*/\1/g')
  TITLE=$(echo "$ENTRY" | grep "title =" | sed -e 's/.*title = "\(.*\)".*/\1/g')

  # Transliterate the title to ASCII
  TASC=$(echo "$TITLE" | iconv -f utf-8 -t iso8859-1//TRANSLIT)

  # Try to look up BibTeX on Google Scholar
  BIB=$(scholar.py -c 1 -t -A "$TASC" --citation=bt)

  # Skip if we got nothing
  [[ -z "$BIB" ]] && continue

  # Output if we got something
  echo -e "FILE: $FILE\n$BIB"
done | tee -a SCHOLAROUT
```

Alternatively, you can search CrossRef, which gives less accurate results but isn't rate-limited:

```bash
grep -n "@misc{zzzz" ArchivedPapers/Bibtex.bib |
cut -d ':' -f 1 |
while read -r NUM
do
  CTX=$(( NUM + 3 ))
  ENTRY=$(head -n"$CTX" ArchivedPapers/Bibtex.bib | tail -n4)
  FILE=$(echo  "$ENTRY" | grep localfile | sed -e 's/.*localfile = "\(.*\)".*/\1/g')
  TITLE=$(echo "$ENTRY" | grep "title =" | sed -e 's/.*title = "\(.*\)".*/\1/g')
  TASC=$(echo "$TITLE" | iconv -f utf-8 -t iso8859-1//TRANSLIT)

  # This is the only difference to the above
  BIB=$(searchtobibtex "$TASC")

  [[ -z "$BIB" ]] && continue
  echo -e "FILE: $FILE\n$BIB"; done | tee -a SCHOLAROUT
```
