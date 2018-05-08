---
title: Git Repositories
---

I try to host Git repositories myself, as far as possible. These pages are
auto-generated from those repos:

```{.unwrap pipe="bash"}
set -e
set -o pipefail

echo "Adding links to repos index.html" 1>&2

find . 1>&2

function genEntry {
  echo "<li>"
    echo "<a href=\"${1}.html\">"
      echo "$1"
    echo '</a>'
    if [[ -f "$keys/$1" ]]
    then
      echo "Found IPNS key for '$1'" 1>&2
      KEY=$(grep '^.' < "$keys/$1")
      printf '(<a href="https://ipfs.io/ipns/%s">IPNS link</a>)' "$KEY"
    fi
  echo '</li>'
}

function genList {
  echo '<ul>'
    while read REPO
    do
      NAME=$(basename "$REPO" .html)
      echo "Found repo '$NAME'" 1>&2
     genEntry "$NAME"
    done < <(find "$repos" -type f -o -type l | sort)
  echo '</ul>'
}

genList | pandoc -f html -t json
```
