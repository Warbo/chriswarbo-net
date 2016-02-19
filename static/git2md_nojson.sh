#!/bin/sh

# Takes a git repo as an argument, spits out some stats and a README

# Filenames to check for a README
READMES="README README.md"

# Default README content
README_MSG="No README found"
README=""

# Prefer looking in local repos, as it's faster
if [ -d "/home/chris/Programming/repos/$1.git" ]
then
    echo "Found local copy of '$1.git'" >> /dev/stderr
    pushd "/home/chris/Programming/repos/$1.git" > /dev/null

    DATE=$(git log -n 1 --format=%ci)
    echo "Latest commit date for '$1.git' is '$DATE'" >> /dev/stderr

    for f in $READMES
    do
        if FOUND=$(git show "master:$f")
        then
            echo "Found '$f' for '$1.git'" >> /dev/stderr
            README="$FOUND"
            README_MSG=$(echo -e "Contents of $f follows\n\n---\n\n")
            break
        fi
    done
    popd > /dev/null
else
    echo "Looking up '$1.git' remotely" >> /dev/stderr

    MASTER=$(wget -O- "http://chriswarbo.net/git/$1/branches/master/index.html")
    DATE=$(echo "$MASTER" | grep -o "<br>Date:[^<]*")
    echo "Latest commit date for '$1.git' is '$DATE'" >> /dev/stderr

    for f in $READMES
    do
        if FOUND=$(wget -O- "http://chriswarbo.net/git/$1/branches/master/$f")
        then
            echo "Found '$f' for '$1.git'" >> /dev/stderr
            README="$FOUND"
            README_MSG=$(echo -e "Contents of $f follows\n\n---\n\n")
            break
        fi
    done
fi

TICK='`'

cat <<EOF
---
title: "$1"
---

*Last updated: $DATE*

${TICK}git clone http://chriswarbo.net/git/$1.git${TICK}

[View repository](/git/$1/)

$README_MSG
$README
EOF
