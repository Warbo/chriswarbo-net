---
title: Site Changes
---

I am a big fan of automation, and have a particular set of scripts for managing
[my git repositories](/projects/repos). In particular, when I push changes to a
repo, they get pushed to my own domain as well as [GitHub](http://github.com),
and the [pages describing them](/projects/repos/) are also updated.

Unfortunately, my site's build process was pushing the whole thing over the
network every time, which made this quite tedious. I've now altered the process
to only rsync across the changed files, so it's much faster now.

The only remaining issue is that I also run a test suite against the site before
pushing any changes, and this is pretty slow. I think I'll split them into
fast/critical tests which run every time, and slow/optional tests which can be
run less frequently.
