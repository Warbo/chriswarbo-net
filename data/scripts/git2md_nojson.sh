#!/bin/sh

# Takes a git repo as an argument, spits out some stats and a README

cd "root/git/$1.git"

TICK='`'
DATE=$(git log -n 1 --format=%ci)

echo "*Last updated: $DATE*"
echo
echo "${TICK}git clone http://chriswarbo.net/git/$1.git${TICK}"
echo

for f in "README"
do
    git show "master:$f"
done
