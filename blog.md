---
title: Blog
packages: [ 'showPosts' ]
content_classes: h-feed
extra_head:
  <link rel="alternate" type="application/rss+xml" href="blog.rss"
        title="ChrisWarbo.net RSS feed"/>
  <link rel="alternate" type="application/atom+xml" href="blog.atom"
        title="ChrisWarbo.net Atom feed"/>

---

Here you can find all of my previous posts. These are also available as a feed,
in [Atom](/blog.atom) format and [RSS](/blog.rss) format.

```{.unwrap pipe="sh | pandoc -t json"}
find root/rendered/blog -type f | grep -v "index.html" |
                                  sort -r              |
                                  showPosts
```
