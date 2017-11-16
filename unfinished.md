---
title: Unfinished
packages: [ 'showPosts' ]
---

Unfinished posts live here so I can preview them:

```{.unwrap pipe="sh | pandoc -t json"}
export BASE_DIR='unfinished'
export STRIP_PREFIX="$unfinishedPages/"
find "$unfinishedPages" -type f -o -type l | sort -r | showPosts
```
