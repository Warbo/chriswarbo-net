---
title: Unfinished
dependencies: static/showPost*
---

Unfinished posts live here so I can preview them:

```{.unwrap pipe="sh | pandoc -t json"}
find root/rendered/unfinished -type f | sort -r | ./root/static/showPosts
```
