#!/bin/sh

# Takes a git repo as an argument, spits out some stats and a README

READMES="README README.md"
README="No README found"

if [ -d "/home/chris/Programming/repos/$1.git" ]
then
    (cd "/home/chris/Programming/repos/$1.git"
     DATE=$(git log -n 1 --format=%ci)
     for f in $READMES
     do
         if FOUND=$(git show "master:$f")
         then
             README="$FOUND"
         fi
     done)
else
    MASTER=$(wget -O- "http://chriswarbo.net/git/$1/branches/master/index.html")
    DATE=$(echo "$MASTER" | grep -o "<br>Date:[^<]*")
    for f in $READMES
    do
        if FOUND=$(wget -O- "http://chriswarbo.net/git/$1/branches/master/$f")
        then
            README="$FOUND"
        fi
    done
fi

TICK='`'

echo "*Last updated: $DATE*"
echo
echo "${TICK}git clone http://chriswarbo.net/git/$1.git${TICK}"
echo
echo "[View repository](/git/$1/)"
echo
echo "$README"
