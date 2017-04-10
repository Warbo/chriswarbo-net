---
title: Unfinished
packages: [ 'showPosts' ]
---

Unfinished posts live here so I can preview them:

```{.unwrap pipe="sh | pandoc -t json"}
cd root/rendered
find unfinished -type f -o -type l | sort -r | showPosts
```
