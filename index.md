---
title: <a class="h-card" rel="me" href="http://chriswarbo.net">Chris Warburton</a>'s Homepage
extra_head:
  <link href="mailto:chriswarbo@gmail.com" rel="me" />

dependencies: rendered/blog.html static/showPost* static/stripTitle
postprocessor: ./static/stripTitle
---

<div style="float: right; margin: 0 10px 10px 10px;" >

```{.unwrap pipe="sh | pandoc -t json"}
./root/static/file2img.sh "Code" < root/static/code.png
```

</div>

I'm a Free Software advocate, Physicist, Computer Scientist and metal head.

This is a place for me to collect thoughts and experiments, mostly around
programming languages and artificial intelligence.

Pages are organised into the following overlapping categories:

## [Blog](/blog.html)

A [FINO](http://en.wikipedia.org/wiki/FINO) stack of rants, hacks,
opinions, etc. Updated frequently but rarely revised.

```{.unwrap pipe="bash | pandoc -t json"}
find root/rendered/blog -type f |
  sort -r                       |
  head                          |
  ./root/static/showPosts
```

## [Essays](/essays.html)

A collection of thought-out, carefully managed articles and demos.
Infrequently updated, but hopefully accurate.
