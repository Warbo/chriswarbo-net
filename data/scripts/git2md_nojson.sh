#!/bin/sh

# Takes a git repo as an argument, spits out some stats and a README

(cd "/home/chris/Programming/repos/$1.git"

 TICK='`'
 DATE=$(git log -n 1 --format=%ci)

 echo "*Last updated: $DATE*"
 echo
 echo "${TICK}git clone http://chriswarbo.net/git/$1.git${TICK}"
 echo
 echo "[View repository](/git/$1/)"
 echo

 for f in "README"
 do
     git show "master:$f"
 done)
