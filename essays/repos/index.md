---
title: Git Repositories
dependencies: static/git2md*
---

I try to host Git repositories myself, as far as possible. These pages are auto-generated from those repos:

```{pipe="sh > repos"}
# Find all repos on chriswarbo.net
wget -O- 'http://chriswarbo.net/git' |
grep -o '<a .*</a>'                  |
grep -o 'href=".*\.git/"'            |
grep -o '".*"'                       |
grep -o '[^"/]*'
```

```{pipe="sh >> /dev/stderr"}
# Render a page for each repo
REPOS=$(cat repos)
cd root
echo "$REPOS" | while read -r REPO
do
    NAME=$(basename "$REPO" .git)
    FILE="rendered/essays/repos/$NAME.html"
    echo "Generating $FILE"
    ./static/git2md_nojson.sh "$NAME"  |
    SOURCE= DEST= ./static/render_page > "$FILE"
done
```

```{.unwrap pipe="sh | pandoc -f html -t json"}
# Put links to each repo on this page
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
