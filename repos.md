---
title: Git Repositories
dependencies: static/git2md*
---

I try to host Git repositories myself, as far as possible. These pages are auto-generated from those repos:

```{pipe="bash > repos"}
echo "Looking up repos from chriswarbo.net/git" >> /dev/stderr

wget -O- 'http://chriswarbo.net/git' |
grep -o '<a .*</a>'                  |
grep -o 'href=".*\.git/"'            |
grep -o '".*"'                       |
grep -o '[^"/]*'
```

```{pipe="bash >> /dev/stderr"}
echo "Rendering a page for each repo"
REPOS=$(cat repos)

# render_page assumes we're in the blog root
cd root

while read -r REPO
do
    NAME=$(basename "$REPO" .git)
    FILE="rendered/projects/repos/$NAME.html"

    if [[ -f "$FILE" ]]
    then
        DATE=$(grep '<p><em>Last updated: ' < "$FILE" |
                   sed -e 's@.*>Last updated: @@g'    |
                   sed -e 's@</em></p>.*@@g')
        echo "Found existing '$FILE', last modified '$DATE'"
    else
        echo "No existing '$FILE' found"
        DATE=""
    fi

    echo "Looking up content for $FILE"
    CONTENT=$(LATEST="$DATE" ./static/git2md_nojson.sh "$NAME")
    CODE="$?"

    if [[ "$CODE" -eq 100 ]]
    then
        echo "Skipping regeneration of '$FILE'"
    else
        echo "Got new content for '$FILE', generating"
        echo "$CONTENT" | SOURCE= DEST= ./static/render_page > "$FILE"
    fi
done < <(echo "$REPOS")
```

```{.unwrap pipe="bash | pandoc -f html -t json"}
echo "Adding links to repos on to rendered/projects/index.html" >> /dev/stderr

echo '<ul>'
while read REPO
do
    NAME=$(basename "$REPO" .git)
    echo "<li><a href=\"${NAME}.html\">"
    echo "$NAME"
    echo '</a></li>'
    #echo '<br />'
done < repos
echo '</ul>'
```

```{pipe="bash >> /dev/stderr"}
# Turn all repos into filenames
FILES=""
while read -r REPO
do
    NAME=$(basename "$REPO" .git)
    FILE="$NAME.html"
    FILES=$(echo -e "$FILES\n$FILE")
done < repos

echo "Making sure all expected repos have pages"
ERR=0
cd root/rendered/projects/repos
while read -r FILE
do
    # Skip empty entries
    [[ -n "$FILE" ]] || continue

    if ! [[ -e "$FILE" ]]
    then
        echo "Couldn't find 'rendered/projects/repos/$FILE'"
        ERR=1
    fi
done < <(echo "$FILES")

echo "Making sure all repo pages are expected"
while read -r FILE
do
    REL=$(basename "$FILE")

    # Skip index.html, which shouldn't correspond to a repo
    [[ "x$REL" = "xindex.html" ]] && continue

    if ! echo "$FILES" | grep -F "$REL" > /dev/null
    then
        echo "Didn't expect to find 'rendered/projects/repos/$REL'"
        ERR=1
    fi
done < <(find . -name "*.html")

exit "$ERR"
```
