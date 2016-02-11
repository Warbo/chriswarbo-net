---
title: Blog
dependencies: static/showPost*
content_classes: h-feed
---

Here you can find all of my previous posts:

```{.unwrap pipe="sh | pandoc -t json"}
find root/rendered/blog -type f | sort -r | ./root/static/showPosts
```
