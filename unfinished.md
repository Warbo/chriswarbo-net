---
title: Unfinished
dependencies: static/showPost*
---

Unfinished posts live here so I can preview them:

```{.unwrap pipe="sh | pandoc -t json"}
cd root/rendered
find unfinished -type f | sort -r | TO_ROOT=. ../static/showPosts
```
