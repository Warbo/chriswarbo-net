---
title: Experiment Dump
---
Over the past few years I've accrued a lot of failed, successful or otherwise
abandoned experimental programs in my
/home/chris/Files/Documents/Play/Programming directory (yes, I am somewhat OCD
about file organisation). Since they might be interesting to some people, as
they were obviously so interesting to me that I wanted to write them, I've
started to upload them to [git](/projects/repos). Here's a list, along with a
brief description.

## [MaxInt.java](/git/java-maxint/git/branches/master/MaxInt.java) ##

This is a simple test showing one of the reasons I hate Java. Java stores
integer numbers in a fixed amount of space, 32 bits. 32 bits can be in one of
2^32 unique combinations, which Java divides down the middle. The middle
combination, 10000000000000000000000000000000, is taken to be zero. The
combination above, 10000000000000000000000000000001 represents 1, and so on up
to 11111111111111111111111111111111, which is 2147483647. In the other
direction, the combination below (01111111111111111111111111111111) represents
-1, 01111111111111111111111111111110 represents -2 and so on until
00000000000000000000000000000000, which is -2147483648.  The problem with this
can be shown if you try to add 1 on to the biggest number, which gives
100000000000000000000000000000000, but since Java only allows numbers to be 32
bits long it only bothers looking at the last 32 bits, so it thinks that
2147483647 + 1 = -2147483648, which in my opinion is a fail. To add insult to
injury, Java doesn't allow applications to compile unless they handle every
possible exception they come into contact with, including those that will never
be thrown or which aren't even used, yet Java is perfectly happy to let its own
failures pass without comment, causing debug headaches.

To run this just compile it (for example with `javac MaxInt.java`), then run it
(for example with `java MaxInt`).

## [java-gnucleon](/git/java-gnucleon) ##

This is a simple board game along the lines of Atoms on the Amiga. Players take
it in turns to click on squares to "add an atom to them". Anyone can click on an
empty square (indicated by a 0), but once a square has an atom in then it is
owned by that player (and changes colour) and only that player can click on it
from then on. Once a square gets as many atoms in it as it has nearest
neighbours (not including diagonals) then it explodes, sending one atom to each
neighbour and claiming them for the player. Chain reactions can occur if a
square explodes and sends an atom to a neighbouring square, giving it enough to
explode, and so on.

## [java-package-installer](/git/java-package-installer) ##

This is a non-functional Java GUI for a package management tool I was working on
a couple of years ago. It's similar to APTonCD, but as far as I know predates it
a little.

## [AppPrefs](/git/python-app-prefs) ##

This is a non-functional Python/GTK GUI for choosing GNOME's default
applications. It shows how the default browser could be chosen, giving a textual
description of each one's unique features (ie. what makes it different) along
with a screenshot. The idea is that users don't need to know or remember the
names of the applications, they can read the description and look a the
screenshot to see if it's the one they were looking for (these days Synaptic can
show screenshots, which is awesome :D ). Also, I wanted to get rid of the IMHO
broken idea of associating applications with filenames, such as files ending in
".mp3" and so on. File types should be determined using magic or libmagic, and
users shouldn't have to care about the implementation. They should just be able
to say "Music" or "Spreadsheet".

## [Bouncy.py](/git/python-graphics-tests/git/branches/master/bouncy.py) ##

A very simple Python script which makes a square bounce around the screen based
on some very dodgy Physics.
