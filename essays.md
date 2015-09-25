---
title: Essays
---

A curated collection of (hopefully) well-maintained ramblings:

```{.unwrap pipe="nix-shell -p xidel --run sh | pandoc -t json"}
function show {
    URL=$(echo "$1" | sed -e 's@.*/essays/@/essays/@g')
    TITLE=$(xidel - -q --extract '/html/head/title/text()' < "$1")
    printf '<li><a href="%s">%s</a></li>' "$URL" "$TITLE"
}

echo '<ul>'
for ENTRY in root/rendered/essays/*
do
    if [[ -d "$ENTRY" ]]
    then
        show "$ENTRY"/index.*
    else
        show "$ENTRY"
    fi
done
echo '</ul>'
```
