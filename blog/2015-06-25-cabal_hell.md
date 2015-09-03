---
title: Cabal Hell
---

I've spent the last few months building a system for processing [Haskell](http://haskell.org) code: specifically, reading in arbitrary Haskell, selecting a bunch of functions, and running them through [QuickCheck](https://hackage.haskell.org/package/QuickCheck). I'm not fussy how this would work; eg. the functions could come from strings of raw Haskell code, filenames, module names or Cabal packages; whichever's easiest.

One of my first attempts was to use [`haskell-src-exts`](https://hackage.haskell.org/package/haskell-src-exts) to parse the code; however, one feature I wanted was "dependency chasing", ie. figuring out how the code's dependencies are being used (ie. if the code uses `foo`, is this from a local let-binding, an argument, a pattern-match, a top-level definition, or a separate module (and if so, which)?). I tried playing with the [haskell-names](https://hackage.haskell.org/package/haskell-names) package, but hit problems to do with package databases (probably exacerbated by the fact I'm using [NixOS](http://nixos.org)).

I gave up that approach and decided to use the [GHC API](https://wiki.haskell.org/GHC/As_a_library) instead. The main reasons for using GHC are:

 - Pretty much guaranteed compatibility, since most Haskell authors will try to get their code working with GHC and not much else; even if that includes [CPP](http://blog.haskell-exists.com/yuras/posts/stop-abusing-cpp-in-haskell.html), [TemplateHaskell](https://wiki.haskell.org/Template_Haskell), [foreign functions](https://wiki.haskell.org/Foreign_Function_Interface), etc.
 - GHC will automatically chase dependencies and imports

In the process I've learned an awful lot about Haskell, GHC, [Cabal](https://www.haskell.org/cabal/) and [Nix](http://nixos.org/nix/), but unfortunately I've still managed to get trapped in depths of [Cabal Hell](http://www.well-typed.com/blog/2014/09/how-we-might-abolish-cabal-hell-part-1/).

Whilst the GHC API is much more cumbersome and fragile than, for example, LISP metaprogramming, it works reasonably well for small, self-contained use-cases; say, conditionally compiling some extra modules bundled with your project. However, when the input doesn't have such constraints, we quickly fall into Cabal Hell.

If we're accepting arbitrary Haskell code in, then dependency chasing will fail most of the time due to missing dependencies. We need some mechanism for identifying and locating dependencies, import paths, etc. In essence, any application trying to use the GHC API internally for arbitrary code must effectively become a specialised version of GHC. This is a problem because GHC is a large application, quite complex, rather ad-hoc in places, and *extremely* configurable. Even if we provide hooks to all of this configuration, we need some way to instantiate it all; effectively, we need need to reimplement Cabal as well!

After fighting with this setup for a while, I eventually caved in and tried writing a GHC Core plugin
