---
title: Procedural Pictures
dependencies: essays/procedural/*
---

I've been playing with procedurally-generated images recently; here are some interesting ones. All functions `f`{.haskell} are mapped over the pixels, taking in the x and y coordinates as bytes and returning a byte for the colour (a byte is a list of 8 `Bool`{.haskell}s).

```{.unwrap pipe="sh | pandoc -t json"}
echo '<ul>'
for FILE in root/essays/procedural/*
do
    NAME=$(basename "$FILE" | rev | cut -d '.' -f 2- | rev)
    TITLE=$(grep "title[ ]*:" < "$FILE" | sed -e 's/.*title[ ]*:[ ]*//g')
    #[[ "x$NAME" = "xindex" ]] && continue
    echo "<li><a href='$NAME.html' title='$TITLE'>$TITLE</a></li>"
done
echo '</ul>'
```
