---
title: Lock Files Considered Harmful
---

This post is about files used by dependency management tools like Yarn, to cache
the results of non-deterministic processes (e.g. querying HTTP servers). This
post is *not* about files used as sentinels to prevent concurrent access!

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
