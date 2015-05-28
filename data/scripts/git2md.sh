#!/bin/sh

# Takes a git repo on stdin, spits out some stats and a README
REPO=$(cat)
./root/data/scripts/git2md_nojson.sh $REPO | pandoc -f markdown -t json
