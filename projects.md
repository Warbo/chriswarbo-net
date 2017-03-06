---
title: Projects
packages: [ 'xidel' ]
---

A curated collection of (hopefully) well-maintained ramblings:

```{.unwrap pipe="sh | pandoc -t json"}
function show {
    URL=$(echo "$1" | sed -e 's@.*/projects/@/projects/@g')
    TITLE=$(xidel - -q --extract '/html/head/title/text()' < "$1")
    printf '<li><a href="%s">%s</a></li>' "$URL" "$TITLE"
}

echo '<ul>'
for ENTRY in root/rendered/projects/*
do
    [[ "x$(basename "$ENTRY")" = "xindex.html" ]] && continue

    if [[ -d "$ENTRY" ]]
    then
        show "$ENTRY"/index.*
    else
        show "$ENTRY"
    fi
done
echo '</ul>'
```
