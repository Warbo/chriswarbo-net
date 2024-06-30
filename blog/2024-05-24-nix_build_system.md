---
title: Nix is a build system
---

After using Nix for a decade, I dislike that it describes itself as a "purely
functional package manager"; that causes all sorts of confusion, since it has
far more in common with something like Make (e.g. see
[Nix from the bottom up](/projects/nixos/bottom_up.html))

Nix has some differences to Make, like caching results using their hash instead
of timestamp, but the main advantage is that its build receipes are composable
(thanks to the FP nature of their definitions).

For example, say I run `make` in some project repo, like Firefox. Make will read
that project's Makefile, which contains elaborate rules for how the various
build products depend on each other. Yet despite all that care and attention, I
get an error: `cc: command not found`. Oops, I don't have a C compiler! So I
grab a copy of the GCC source, and what do I find inside? Another Makefile! The
`cc` command required by the Firefox Make rules is itself defined with Make
rules; but the Firefox Makefile can't refer to them, since Make is not
composable.

In contrast, Nix is composable: Nix definitions can `import` other files,
including from build outputs! For example, we can write a build receipe which
imports its definition from a build output; where that build fetches a git
commit; and the definitions inside import things from some other builds; and
those download and extract a bunch of .tar.gz files; and so on.

Nixpkgs is the most obvious example of this composability, with mountains of
build receipes, built up from a relatively small "bootstrap" (pre-built binaries
for a few basic tools, like parts of GNU). It's also a testament to
backwards-compatibility, since it features build receipes (and helper functions)
which act as wrappers around all sorts of legacy tools like Make, PIP, NPM,
Cargo, Cabal, etc. (which is useful if you're working on a project that's
already sunk a lot of time on such things).

Whilst Nixpkgs provides support for all of these things; Nix itself is only
capable of invoking a single `exec` syscall (again, see
[Nix from the bottom up](/projects/nixos/bottom_up.html)). Everything else is
built up on that foundation, and isn't tied to any particular tool, language,
framework, etc.

Hence it's not so much that Nix is a "package manager", or "orchestration" tool,
or "configuration manager", etc. It's more like: those categories of tools are
workarounds for crappy, non-composable build tools like Make. Nix is a
composable build tool, so all of those other things turn out to be unnecessary.
