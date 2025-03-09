---
title: Active Code Updates
---

[GHC 8](https://www.haskell.org/ghc) has been out for a while now, and I've been
tentatively upgrading. Since I'm using [Nix](http://nixos.org/nix), it's trivial
to use different versions of GHC for different applications/projects, but it
would be nice to have all of my own stuff working at least. To investigate this,
I've had my [Hydra](http://nixos.org/hydra) continuous integration server build
all of my projects using all versions of GHC >= 7.8 which are available
in [nixpkgs](http://nixos.org/nixpkgs); this includes 7.8.3, 7.8.4, 7.10.2,
7.10.3, 8.0.1 and 8.0.2. A `ghcHEAD` version is also available, but I've had to
filter out things like the `integer-simple` and `ghcjs` variants as the packages
seem to be broken.

One thing I learned in the process is that
my [active code](/projects/activecode) scripts don't work with the latest (1.19)
version of [pandoc](http://hackage.haskell.org/package/pandoc)! It looks like
the JSON format has changed. This isn't so much of a problem: I can just force a
working version (1.17) in the dependencies of PanPipe and PanHandle.

It turns out that the Cabal files for these projects were a bit of a mess! They
date back to my first forays into [Cabal](https://www.haskell.org/cabal), and it
appears I'd used `cabal init` to generate a package with a bunch of example
dependencies, and never removed those examples!

These packages are all cleaned up now, and pushed to [git](/projects/repos).
I've set the version bounds on their other dependencies (including `base`) such
that they'll hopefully build with GHC versions between 7.8 and 8.0; Hydra's
currently churning through them :)

I've also taken this opportunity to finally sort out the naming of these
packages. They are now `panpipe` and `panhandle`, which is in line with
`pandoc`. Whilst their English names are still "PanPipe" and "PanHandle" (as
well as their module names!) there's no longer a `PanPipe` package name, and
I've tried to remove all remaining references to the original name "pan handler"
(which is a noun rather than a verb).

Finally, both [PanPipe](http://hackage.haskell.org/package/panpipe)
and [PanHandle](http://hackage.haskell.org/package/panhandle) have been pushed
to [Hackage](http://hackage.haskell.org). This makes them easier to discover and
install, via Cabal and Nix (and maybe even [Stack](https://haskellstack.org)) :)
