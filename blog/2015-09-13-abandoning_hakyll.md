---
title: Abandoning Hakyll
---

For a while I've been using [Hakyll](http://jaspervdj.be/hakyll/) to manage this
site, but I've recently switched over to
[Make](https://www.gnu.org/software/make/).

Whilst Hakyll's use of Haskell certainly makes it nice to abstract over and
compose common functionality, I've found that pretty much all of that
functionality is boilerplate; trivially expressed in other ways, eg. shell
commands. For example, Hakyll uses [Pandoc](http://pandoc.org/) to render pages,
which is an excellent choice. However, since making
[my Pandoc-based active code system](/projects/activecode/), I've replaced
Hakyll's use of Pandoc with a shell invocation. Likewise, every other feature of
Hakyll I was using turns out to be a boilerplate-heavy reimplementation of tasks
which are trivial to perform with shell commands; especially so considering that
I can use [PanPipe](/git/panpipe) to run shell commands during rendering (that's
how pages like my [blog post listing](/blog.html) are now implemented).

The only feature that's non-trivial is the cache used for deciding when to
build/rebuild something. However, there's already a widely used, dedicated tool
for doing exactly that, which integrates well with shell commands: Make. I know
it's clunky and awkward at times, but I figured I'd go with the most widely-used
Free Software implementation to start with, and only switch to something else if
I deem it necessary. So far, the dependency ordering of Make is much more
reliable than Hakyll's (I'd usually do a full rebuild with Hakyll; not so with
Make).

I've not quite ported *everything* over; for example, I need to put some scripts
in the RSS and ATOM templates to scan for blog posts, but it certainly seems
much nicer and more self-contained to embed scripts where they're needed, rather
than setting up elaborate template systems.
