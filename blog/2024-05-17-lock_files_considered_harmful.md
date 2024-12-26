---
title: Lock Files Considered Harmful
---

This post is about files used by dependency management tools like Yarn, to cache
the results of non-deterministic processes (e.g. querying HTTP servers). This
post is *not* about files used as sentinels to prevent concurrent access!

I originally wrote this as part of my
[dependency solving in Nix](/projects/nixos/nix_dependencies.html) page, but
decided to make it a stand-alone blog post since (a) that page is already way
too long, and (b) this is a standalone argument which isn't specific to any
particular implementation (i.e. it doesn't *require* Nix; though that was
certainly my motivation in writing this!)

## TL;DR Why Avoid Lock Files? ##

Lock files are a hack to minimise the use of brittle, slow and insecure legacy
tooling. They complicate the development process, and require extra manual
interventions. Their presence is a symptom of poor design: good tools should be
reliable, fast and secure.

If our dependency-solving algorithm is *deterministic*, and its inputs are
*reproducible* (e.g. tracked by a git repo), then lock files are
*unnecessary*. We can still use them to speed up builds, but that's just
ordinary caching, which most build tools already do (to varying extents), and
*does not* require manual intervention.

## Design Principles To Make Lock Files Unnecessary ##

### Reproducible Inputs ###

Input data, *especially* descriptions of available dependencies, should be
specified such that we can reproduce it for any previous run. *This* is the
problem many legacy systems struggle with: e.g. deferring important details to
some HTTP response, with no way to validate its consistency (it's usually *not*
consistent, due to new versions being included in queries!). This is the main
problem with legacy tools, which makes their users resort to lock files.

One easy way to ensure consistency/reproducibility is to reference external
things with a hash, rather than relying on some arbitrary name or HTTP URL that
isn't dependent on the content. Hashes can be given alongside such legacy
mechanisms, to validate the response is as expected; or used directly as an ID
to look up. Git is a popular way to do this (its commit IDs are content hashes),
although other Merkle-trees/chains like IPFS, BitTorrent, etc. may be better at
distribution.

This reproducibility and validation not only avoids some supply-chain attack
vectors, but also allows caching, self/distributed hosting, offline use, and
avoids relying on centralised infrastructure (both physical, like servers and
SSL certificates; but also social, like copyright waivers, content policies,
etc.)

### Determinism ###

Dependency solvers (like almost all software) should be *deterministic*: running
the same executable with the same data should always produce the same output. In
this case the data includes our constraints, as well as all of the available
packages. The output is a set of packages satisfying the constraints.

Lock files are a crutch for non-deterministic processes; they make downstream
steps reproducible, at the cost of an extra dependency (the contents of the lock
file). At best they are unverifiable, unreproducible build artifacts with no
corresponding source; at worst they are plausibly-deniable attack vectors. In
this sense, they embody all the same anti-patterns, foot-guns and time-bombs as
other toxic practices like e.g. Docker images.

Ideally a solver's determinism will be robust against "irrelevant" differences:
e.g. running different compilations of the same source; running on different CPU
architectures and operating systems; and even running different versions of the
solver, if no changes have been made to the underlying algorithm.

I'm not aware of any dependency managers which literally arrive at different
solutions each time; which is reassuring! Still, it's an important baseline.

One edge-case worth mentioning is randomised algorithms, e.g. using some
Monte-Carlo method to avoid brute-force search. That's certainly useful, but
*pseudo-randomness* is usually enough. Personally, I tend to seed them with a
cryptographic hash of the input (for robustness against malicious data) and a
counter (to allow "retries").

### Update 2024-12-26 ###

This recently got quite a few comments
[on Lobste.rs](https://lobste.rs/s/chofar/lock_files_considered_harmful) and a
couple [on Hacker News](https://news.ycombinator.com/item?id=42498836). Whilst
some people grokked what I'm saying, it seems like there was a lot of
misunderstanding, so here's an editorialised FAQ:

#### Weren't things worse before we used lock files? ####

Yes, they were. We're not going back. Instead, we should *continue forwards*,
into a world where lock files *are not necessary*.

The most obvious example of a system which doesn't need lock files is Nix and
its spinoffs (which [I have written about extensively](/projects/nixos)). Whilst
many complain about Nix's UI (its CLI, documentation, Nixpkgs repo, etc.),
[the underlying *idea* of Nix](/projects/nixos/bottom_up.html) is pretty simple
(it doesn't even have any concept of "package" or "version"!), and it's been
around for a couple of decades at this point. Newer package managers could learn
from those ideas; but unfortunately many don't, instead copying the same broken,
"legacy" approaches seen elsewhere, and hence needing hacky mitigations like
lock files.

We can do better.

#### If we write down hashes, isn't that just a lock file? ####

The "lock files" this post is arguing against are auto-generated files, spat out
by some un-reproducible command, and checked into version-control. The problems
they cause are due to not representing any human-made decisions. In contrast, a
human programmer deciding to write some hashes in a file *do not* cause these
same problems (those are just another part of a project's source code).

As an example, consider code review: if changes are made to a hand-written file,
we can ask the author why they made those changes, discuss alternatives, debate
pros and cons, etc. just like any other source file. On the other hand, a
changed lock file doesn't really have an "author": whoever's committing it may
have no idea why certain changes appear, and figuring it out feels more like
reverse-engineering or debugging. (This also makes lock files attack vectors.)

#### Version numbers, ranges, etc. ####

*LOADS* of comments talked about version numbers, version ranges (both for and
against), preferred ways of choosing version numbers (latest versus mininum),
etc. Whilst those are interesting debates to be sure, they're not particularly
relevant to anything I'm saying here. In particular, those are relevant to
questions like "how will the result change, when the solver is run against a
database with updated packages?".

That is *not* what this post is asking. Instead, I'm asking a question more like
"how can we run the solver against a database with *exactly the same* packages
every time?". Unfortunately, many packaging tools (those I refer to as "legacy")
do not support such a seemingly-obvious feature!

#### Doesn't this force centralisation? Isn't this too rigid/inflexible? ####

Nothing in this post is specific to centralised package databases (Hackage,
PyPI, crates.io, Nixpkgs, etc.). Nothing says *how* we should specify
dependencies, whether using version ranges, static hashes, or whatever. Use any
amount of resolving, overriding, constraint-solving, etc.

All I'm asking for, is that we specify enough information to compute the set of
files we need. (Lock files record the result of that computation, which is not
the same thing; since it's not reproducible!)

#### What about libraries, whose dependencies may need more flexibility? ####

Again, nothing in this page is specific to libraries, applications, or indeed
any particular mechanism for specification, resolution, unification, overriding,
etc. Implement those however you like, so long as the dependencies of a build
can be deterministically computed from the source. That can include the sources
of the dependencies, if you like; you do you, so long as it's deterministically
computable, without relying on some unspecified, mutable, external state.
