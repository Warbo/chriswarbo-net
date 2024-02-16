---
title: Dependency solving in Nix
---

I've added [Dependency solving in Nix](/projects/nixos/nix_dependencies.html) to
[my Nix pages](/projects/nixos). It's about using Nix for projects which rely on
legacy "package managers". The latter have two major problems, which complicate
the software supply chain and weaken security and reproducibility:

 - Dependencies are referenced by a user-chosen "name" and "version", like
   `my-json-parser-1.2`. Resolving these to a specific artifact (like a
   precompiled library or source tarball) requires trust, since it's trivial for
   malicious code to *claim* that it's `my-json-parser-1.2`.
   - Compare this to Nix, which using hashes that uniquely specify the content
     and cannot be spoofed. This allows trustless distribution, e.g. via caches
     or even P2P networks (at least when it comes to sources).
 - Many legacy "package managers" also allow dependencies to be specified with a
   pseudo-numerical "range" of versions. Choosing a particular version for each
   dependency requires running a constraint solver, to find an assignment that
   satisfies all of the specified ranges simultaneously, and transitively.

There seems to be a mistaken belief that Nix cannot run such constraint solvers.
I don't know where that idea originated, but it's demonstrably false, since I've
been doing it for years with various package managers (Cabal, Maven, SBT, etc.).

That's why I've written that page, so I can link to it if I see anybody else
make such claims!
