---
title: Haskell Parsing
---
> non-Lispy languages can benefit from macros by creating a skeleton syntax that is slightly above the level of lexer tokens, but below the level of an AST, in terms of meaning

Thanks for pointing this out; I wasn't aware of the phrase "skeleton syntax", but the idea is something I've been ranting about for a while now :)

There are perfectly arguable reasons for avoiding s-expressions (e.g. "hard for people to read"), but most languages seem to 'throw the baby out with the bathwater' by having their human-friendly language be the *only* representation.

This has obvious problems for macros (since macros are "new syntax", they're unparseable using the existing grammar), but it also makes life harder for tool writers (linters, documentation extractors, coverage analysers, etc.) since everyone has to use a full-blown parser, and things might break due to irrelevant changes in the surface syntax (tasks like getting a list of function names shouldn't break just because, say, a new language version adds a shorthand for pattern-matching).

I've been doing a lot of work in Haskell recently, and the situation there is just a mess. There are 3 de facto parsers and AST representations: `haskell-src-exts` is the recommended library, whilst the Template Haskell macro system uses its own, and the GHC compiler defines its own as well. This is especially crazy given that:

 - There shouldn't have been a need to make `haskell-src-exts`, considering that GHC predates it and is written in Haskell.
 - The likely reason for avoiding GHC's implementation is that it depends heavily on side effects (e.g. incorrect `DynFlags` cause [the impossible to happen](https://stackoverflow.com/questions/9242996/how-to-handle-panic-the-impossible-happened-and-continue-in-haskell)).
 - One of the main benefits to using Haskell in the first place is to avoid side effects, especially in pure calculations/transformations.
 - Parsing is a common example of what Haskell and pure functions are good at (even from critics, e.g. "purity is only good for things like parsing, not XYZ")!
 - Another reason to avoid GHC's parser is that the resulting ASTs are full of `undefined` values, which cause a hard crash if accessed (e.g. by a generic AST traversal). Basically like uncheckable, uncatchable null reference exceptions (although they behave differently from a logical, Curry Howard perspective)
 - Another benefit of Haskell is that [it doesn't have null](http://www.nickknowlson.com/blog/2013/04/16/why-maybe-is-better-than-null/)
 - Template Haskell, which defines a completely bespoke set of types for representing Haskell ASTs, is part of GHC. They couldn't even re-use the ASTs *in the same application*.
 - A large proportion of Haskell code can't be parsed by *any* of these, due to a heavy reliance on tools like the C preprocessor; of course, these are only used because Haskell isn't amenable to macros! (TemplateHaskell runs quite late in the compilation process, and hence it can't be used for things like conditional imports, or using a different name based on which version of a library is being used).
