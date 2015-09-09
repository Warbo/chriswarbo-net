---
title: Blog
dependencies: static/showPost*
---

Here you can find all my previous posts:

```{.unwrap pipe="sh | pandoc -t json"}
find root/rendered/blog -type f | sort -r | ./root/static/showPosts
```
