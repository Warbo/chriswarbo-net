---
title: Haskell Parsing
---

I like [s-expressions](2017-08-29-s_expressions.html), but there are
perfectly good arguments for avoiding them (e.g. "hard for people to read"), but
most languages seem to 'throw the baby out with the bathwater' by having their
human-friendly language be the *only* representation!

This has obvious problems for macros (since macros are "new syntax", they're
unparseable using the existing grammar), but it also makes life harder for tool
writers (linters, documentation extractors, coverage analysers, etc.) since
everyone has to use a full-blown parser, and things might break due to
irrelevant changes in the surface syntax (tasks like getting a list of function
names shouldn't break just because, say, a new language version adds a shorthand
for pattern-matching).

A few years ago I did a lot of work in Haskell, and the situation there was just
a mess. There were 3 de facto parsers and AST representations:
`haskell-src-exts` as the recommended library, whilst the Template Haskell macro
system used its own, and the GHC compiler defined its own as well. This was
especially crazy given that:

 - There shouldn't have been a need to make `haskell-src-exts`, considering that
   GHC predates it and is written in Haskell.
 - The likely reason for avoiding GHC's implementation is that it depends
   heavily on side effects (e.g. incorrect `DynFlags` cause [the impossible to
   happen](https://stackoverflow.com/questions/9242996/how-to-handle-panic-the-impossible-happened-and-continue-in-haskell)).
 - One of the main benefits to using Haskell in the first place is to avoid side
   effects, especially in pure calculations/transformations!
 - Parsing is a common example of what Haskell and pure functions are good at
   (even from critics, e.g. "purity is only good for things like parsing, not
   XYZ")!
 - Another reason to avoid GHC's parser is that the resulting ASTs are full of
   `undefined` values, which cause a hard crash if accessed (e.g. by a generic
   AST traversal). Basically like uncheckable, uncatchable null reference
   exceptions (although
   [they behave differently](2020-02-09-bottom.html) from a logical,
   Curry-Howard perspective)
 - Yet, one of the benefits of Haskell is that [it doesn't have
   null](http://www.nickknowlson.com/blog/2013/04/16/why-maybe-is-better-than-null/)!
 - Template Haskell, which defines a completely bespoke set of types for
   representing Haskell ASTs, is part of GHC. They couldn't even re-use the ASTs
   *in the same application*!
 - A large proportion of Haskell code can't be parsed by *any* of these, due to
   a heavy reliance on tools like the C preprocessor; of course, these are only
   used because Haskell isn't amenable to macros! (TemplateHaskell runs quite
   late in the compilation process, and hence it can't be used for things like
   conditional imports, or using a different name based on which version of a
   library is being used).

Thankfully the situation seems to have improved somewhat in the past few years,
with GHC's API becoming saner, and hence more reliable. I may have to revisit my
old tooling, to see if it can take advantage of this!
