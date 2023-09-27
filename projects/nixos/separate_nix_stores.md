---
title: Using Nix with separate stores
---

The Nix build system uses a directory it calls "the store" to keep its
"derivations" (build commands, written in text files with names suffixed by
`.drv`) and "outputs" (the files and directories resulting from a build). By
default, the Nix store is `/nix/store`; in this post I'll show various ways to
change this, using the example of `/my/nix/store` instead.

### A note on `.nix` files ###

Nix operates in two "phases": evaluating and building (which can be mixed
together using features like import-from-derivation, recursive-nix, etc.).
Roughly speaking, evaluation turns `.nix` files into `.drv` files, and building
turns `.drv` files into build outputs (say, a Python library or Firefox binary).
Nix will always use the store to read/write `.drv` files; but `.nix` files can
live anywhere. For example, it's common to use `.nix` files from somewhere in
our home folder (e.g. some uncommitted changes to a project we're working on).

## When is this useful? ##

Changing the Nix store is very rarely needed, and will probably cause headaches
in unexpected ways. Still, we may occasionally find a situation where it's less
trouble than the alternatives. For example:

 - We're using a restricted user account, which can only write to certain places
 - Our usual Nix store is running out of space
 - Re-using the store of some other Nix installation (e.g. from a broken PC)
 - Keeping secret/proprietary files separated from our day-to-day Nix usage
 - Using unreliable/removable/remote storage without breaking our installation

## The `/nix/store` path versus the `/nix/store` directory ##

We can group together all the methods of changing the Nix store into two
categories, which I'll call *changing the store path* and *changing the store
directory*.

Changing the Nix store *path* replaces usages of `/nix/store` with our desired
alternative, say `/my/nix/store`. This seems like an obvious solution, but is
actually a **very bad idea**! The reason is that Nix uses the store path to
identify derivations, outputs and their contents.
