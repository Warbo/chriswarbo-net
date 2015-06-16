#!/bin/sh

# The definitive copy of everything is online
LIST=$(wget -O- 'http://chriswarbo.net/git' | grep -o '<a .*</a>')
ONLINE=$(echo "$LIST"              |
         grep -o 'href=".*\.git/"' |
         grep -o '".*"'            |
         grep -o '[^"/]*')

# Many projects are cached locally before going online
LOCAL=$(ls /home/chris/Programming/repos)

# All of the repos we have pages for
GOT=$(ls essays/repos)

# Ensure all online repos have pages
for REPO in $ONLINE
do
    NAME=$(basename "$REPO" .git)
    if echo "$GOT" | grep "^$NAME.md$" > /dev/null
    then
        true
    else
        echo "'$NAME' is online but not in essays/repos"
    fi

    # Also make sure it's cached locally
    if echo "$LOCAL" | grep "^$REPO$" > /dev/null
    then
        true
    else
        echo "'$REPO' is online but not in Programming/repos"
    fi
done

# Ensure all local repos are online
for REPO in $LOCAL
do
    NAME=$(basename "$REPO" .git)
    if [ ! "x$REPO" = "x$NAME.git" ]
    then
        continue
    fi

    if echo "$ONLINE" | grep "^$REPO$" > /dev/null
    then
        true
    else
        echo "'$REPO' is in Programming/repos but not online"
    fi
done

# Ensure all pages correspond to online repos
for REPO in $GOT
do
    NAME=$(basename "$REPO" .md)
    if [ "x$NAME" = "xindex" ]
    then
        continue
    fi

    if echo "$ONLINE" | grep "^$NAME.git$" > /dev/null
    then
        true
    else
        echo "'$NAME' is in essays/repos but not online"
    fi
done
