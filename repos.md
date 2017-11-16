---
title: Git Repositories
---

I try to host Git repositories myself, as far as possible. These pages are
auto-generated from those repos:

```{.unwrap pipe="bash | pandoc -f html -t json"}
set -e
echo "Adding links to repos index.html" 1>&2

find . 1>&2

echo '<ul>'
  while read REPO
  do
    NAME=$(basename "$REPO" .html)
    echo "Found repo '$NAME'" 1>&2
    echo "<li>"
      echo "<a href=\"${NAME}.html\">"
        echo "$NAME"
      echo '</a>'
      if [[ -f "$keys/$NAME" ]]
      then
        echo "Found IPNS key for '$NAME'" 1>&2
        KEY=$(grep '^.' < "$keys/$NAME")
        printf '(<a href="https://ipfs.io/ipns/%s">IPNS link</a>)' "$KEY"
      fi
    echo '</li>'
  done < <(find "$repos" -type f -o -type l | sort)
echo '</ul>'
```
