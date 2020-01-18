---
title: <a class="h-card" rel="me" href="http://chriswarbo.net">Chris Warburton</a>'s Homepage
extra_head:
  <link href="mailto:chriswarbo@gmail.com" rel="me" />
  <link rel="alternate" type="application/rss+xml" href="blog.rss"
        title="ChrisWarbo.net RSS feed"/>
  <link rel="alternate" type="application/atom+xml" href="blog.atom"
        title="ChrisWarbo.net Atom feed"/>

postprocessor: stripTitle
packages: [ 'file2img', 'showPosts', 'stripTitle' ]
dependencies: [ 'static/code.png', 'static/rants' ]
---

<div style="float: right; margin: 0 10px 10px 10px;" >

```{.unwrap pipe="sh | pandoc -t json"}
file2img "Code" < root/static/code.png
```

</div>

I'm a Free Software advocate, Physicist, Computer Scientist and metal head. I'm
currently a PhD student in Computer Science at the
[University of Dundee](https://www.computing.dundee.ac.uk/about/staff/124).

This is a place for me to collect thoughts and experiments, mostly around
programming languages and artificial intelligence.

Pages are organised into the following overlapping categories:

## [Blog](/blog/)

A [FINO](http://en.wikipedia.org/wiki/FINO) stack of rants, hacks, opinions,
etc. Updated frequently but rarely revised.

```{.unwrap pipe="bash | pandoc -t json"}
export BASE_DIR='blog'
export STRIP_PREFIX="$blogPages/"
find "$blogPages" -type f -o -type l |
  grep -v "index.html"               |
  sort -r                            |
  head                               |
  showPosts
```

[More...](/blog/)

## [Projects](/projects/)

A collection of thought-out, carefully managed articles and demos.
Infrequently updated, but hopefully accurate.

[More...](/projects/)
