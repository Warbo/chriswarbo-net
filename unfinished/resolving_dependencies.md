---
title: Resolving dependencies
---

TODO: These are taken from Dependency solving in Nix, but were a bit too ranty.
I like ranting, so I want to keep these; but need to work them into a coherent,
standalone post.

 - Conversely, why it's actually *really hard* to make legacy "package managers"
   solve constraints (spoiler alert, the difficulty is in setting up the problem
   you want solved; especially in a way which can't be messed up by the operator
   of some third-party HTTP server)

## Dependency solving and package management ##

The idea of "dependency solving" is quite a nuanced and difficult topic: I'm not
going to talk about the constraint-satisfaction aspect, since modern solvers are
pretty good; or the merits of this-or-that policy over "version numbers"; but
rather we're going to focus on the management of multiple, incompatible naming
systems, and how to implement their resolution in a secure, robust,
reproducible and verifiable way.

The idea of "package management" is essentially a way for projects to
under-specify their dependencies: instead of referring to some specific piece of
code, like a particular commit ID of a Git repository, or a `.tar.gz` identified
by a SHA256 hash of its contents, they instead use an arbitrary "name" string
like `http-lib`. These strings must somehow be resolved to a specific artifact;
sometimes with further refinements like "versions" (again, just arbitrary
strings); and perhaps a constraint satisfaction system to narrow-down the
acceptable versions. The usual solution to this (artificial) problem is to pass
the buck to some third-party HTTP server (usually identified via a DNS name,
adding to the abstractions)!

This whole approach to software distribution is inherently insecure
(supply-chain attacks, name squatting, cache poisoning), unreliable (beholden to
some particular HTTP server being up, with an up-to-date SSL certificate, etc.),
and slow (requiring round trips over the network). Evidence of these weaknesses
is clear from the widespread deployment of workarounds, like private resolvers,
"offline modes" and "lock files"; which bypass the full resolution of names by
caching expected results. Rather than trying to work around these issues, this
post tries to tackle them head-on (which is why the examples get a little...
messy).
