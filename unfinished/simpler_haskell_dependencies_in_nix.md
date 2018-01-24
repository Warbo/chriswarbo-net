---
title: Simpler Haskell Dependencies in Nix
---

I really like the [Nix]() language/package-manager/build-system/ecosystem (I
might like [Guix]() even more, but I'm spinning too many plates to switch at the
moment).

One interesting thing about Nix is that it doesn't have any dependency solver.
In systems like [dpkg]() and [cabal](), each package has a global name (e.g.
`firefox`) and a list of dependencies (e.g. `[network-stack, audio-player]`),
possibly with version constraints (e.g. `audio-player > 1.3`).

When we ask to install a package (like `firefox`), a [constraint solver]() is
run to try and find a (minimal) set of packages to install, which includes the
package we asked for, as well as allowed versions of each dependency, and their
dependencies, and so on [transitively](). Without a constraint solver, those
packaging systems would be incredibly tedious to manage, since all of the
hard-coded versions would have to match up, making updates quite laborious.

Nix avoids this problem by providing a rich programming language: packages
aren't just global names with a list of package names they depend on. Rather, we
write *functions* in the Nix language, which will *calculate* a package, based
on various inputs (such as dependencies). If we run these functions on different
inputs, we can get a package with different dependencies, without having to
manually maintain cohesive versions of everything.

Still, the way we use Nix in practice can be a little frustrating. In the case
of Haskell packages, those in the [`nixpkgs` repository]() *are* provided in the
form of functions which stitch together their dependencies automatically, but we
still need to pick cohesive versions of everything since, as mentioned above,
there is no dependency solver in Nix.

Personally, I think this is rather futile: we're trying to maintain a huge
collection of Haskell packages, such that none of them conflict with any others.
Yet we never actually need *all* of those packages at the same time: we might
only depend on a handful.

My solution to this is to give Nix a dependency solver for Haskell packages.
That sounds heavy-handed, but it's actually quite simple: Nix can run arbitrary
shell commands, so we just tell Nix to run `cabal`!

I experimented with using [`tinc`]() for this, since it has rudimentary Nix
integration. However, despite [hacking it into submission](), it was still not a
particularly good fit for this job.

Since then, I've switched to using [cabal sandboxes](), and this seems to work
well. Here's a brief description.
