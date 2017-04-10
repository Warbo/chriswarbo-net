---
title: Git Repositories
packages: [ 'git2md', 'wget' ]
---

I try to host Git repositories myself, as far as possible. These pages are
auto-generated from those repos:

```{.unwrap pipe="bash | pandoc -f html -t json"}
echo "Adding links to repos index.html" 1>&2

echo '<ul>'
while read REPO
do
    NAME=$(basename "$REPO" .html)
    echo "<li><a href=\"${NAME}.html\">"
    echo "$NAME"
    echo '</a></li>'
    #echo '<br />'
done < <(find ./root/repos -type f -o -type l | sort)
echo '</ul>'
```
