---
title: Free Elves
---

I'm a big fan of embedding arbitrary, possibly effectful, programs inside pure
languages through the use of free (and "freer") constructions, like free
applicatives and free monads.

This raises an interesting question: if we can use an "embedded domain-specific
language" to program in a "host" (e.g. Haskell) as if it were a "target" (e.g.
SQL); can we take existing programs from the outside world, and embed them as
values in the host language?

For example, say we want to integrate a Haskell program with a bash script. The
most naïve thing to do would be to write the script to a file, then have our
Haskell program call out to the `bash` program, passing the path to the script
file as an argument.

A better approach would store the script in our Haskell code, call out to `bash`
as before, and pipe the script into the subprocess's standard input. This way,
we move some of the responsibilities into our own program: for example, because
there is no file, we don't have to rely on the `bash` command to handle it
correctly, or for the OS to give us read permission, etc.

An even better approach, at least until [languages play nicely together](
http://link.springer.com/chapter/10.1007/978-3-642-14107-2_19), would keep the
script as a separate file, but read it in *at compile time*. This way, the
difficulties associated with locating and reading the file are only dealt with
once, in a nicely self-contained way (i.e. if there's a problem, we just abort
the compilation; we're not in the middle of some big job that needs tearing down
correctly). Note that while Haskell doesn't have a "compile-time phase" (unlike,
say, [Zig](http://ziglang.org)), we can use Template Haskell to mess around with
the syntax tree during compilation, which seems to be enough for our purposes.

If reading in the script is preferable, why don't we go one step further and
read in the contents of the `bash` command too? We can treat the resulting value
as a serialised form of some more-Haskell-friendly `ELF` datatype, which we can
interpret using some form of free construction; with appropriate functions for
running, providing an environment, etc. We can then ensure that the correct
`bash` will be given the correct script, executed using the correct semantics,
in an isolated/testable way, and so on, all without having to interact with the
outside world at run-time.

This idea struck me as a little outlandish to begin with; but then I remembered
that the [Truffle implementation of Ruby does a similar thing](
http://chrisseaton.com/rubytruffle/cext), although they implement C and Ruby in
the same VM, rather than implementing ELF (or similar) in Haskell. In fact,
their code can actually be *faster* than just calling out to C via a foreign
function interface, since their JIT compiler has full access to all of the Ruby
and C, which allows optimisations to fuse things together.

A similar thing could be done with my hypothetical ELF embedding: we *could*
treat the `bash` code as a "black box"; we could instead "deserialise" it into
a datastructure with semantically-meaningful operations, which can be optimised
in some way.

In the above example, the `bash` executable, the script and the environment
(provided by our interpreter function) are all known at compile time, so we can
partially-apply bash to the script, supercompiling all of the parsing
functionality to produce a combined 'bash+script' which knows exactly what it
needs to execute when invoked. More aggressive optimisations might use the known
features of the pure environment to shortcut the highly dynamic nature of bash,
since most of the features will be known to be unused.

A similar approach can be taken to embed with other languages, e.g. [Python](
https://github.com/mattgreen/hython), [Perl](
https://hackage.haskell.org/package/Pugs), or indeed [Haskell](
https://www.haskell.org/ghc).

Note that I've used Haskell here as an example; if I were really going to
implement such a thing, I'd maybe target something more low-level (e.g. without
garbage collection, RTS, etc.) so that I could tease out as much performance as
possible. Languages like [Zig](http://ziglang.org),
[Terra](http://terralang.org), [Nim](https://nim-lang.org) or
[Rust](https://www.rust-lang.org). Another possibility would be to use the host
language (e.g. Haskell) as a code generator, *outputting* an ELF executable. In
the same way that combinators can splice together strings, or plumb together
parsers, we could make combinators for fusing together executables.

This is pretty much an extension of [my earlier thoughts on annotating values as
pure](/blog/2017-02-13-what_i_would_like_in_nix.html) to allow more aggressive
optimisation.
