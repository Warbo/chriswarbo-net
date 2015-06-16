---
title: Git Repositories
---

I try to host Git repositories myself, as far as possible. These pages are auto-generated from those repos:

```{.unwrap pipe="sh | pandoc -f html -t json"}
REPOS=$(wget -O- 'http://chriswarbo.net/git' |
        grep -o '<a .*</a>'                  |
        grep -o 'href=".*\.git/"'            |
        grep -o '".*"'                       |
        grep -o '[^"/]*')

echo '<ul>'
echo "$REPOS" | while read REPO
                do
                  NAME=$(basename "$REPO" .git)
                  echo "<li><a href=\"${NAME}.html\">"
                  echo "$NAME"
                  echo '</a></li>'
                  #echo '<br />'
                done
echo '</ul>'
```
