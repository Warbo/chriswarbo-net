#!/usr/bin/env bash
set -e

# Render a specific page, and when finished open it in a browser (and make a
# notification sound). Useful in combination with tools like entr, to reload
# when saving changes.

browseEWW() {
    emacsclient -eval "(eww \"file://$1\")"
}

show() {
    if command -v firefox > /dev/null
    then
        firefox "$1"
    else
        if hostname | grep -q amd64
        then
            F="~/repos/chriswarbo-net/GOT.html"
            ssh pinephone "rm -f $F"
            cp -v "$1" ./GOT.html
            ssh pinephone "DISPLAY=:0 ~/.nix-profile/bin/firefox $F"
        else
            echo "$1"
        fi
    fi
}

[[ -n "$PAGE" ]] || PAGE='unfinished.unfinished."falsifying_myself.html"'
F=$(./render page "$PAGE") && show "$F"
command -v pop > /dev/null && pop
hostname | grep -q amd64 && ssh pinephone "~/.nix-profile/bin/pop"
true
