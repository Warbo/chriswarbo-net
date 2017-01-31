---
title: Querying Impurities
---

If a computation is "pure", it has no side-effects: the only thing it does is to
create its output, and that output depends only on the computation's input. Pure
computations can't, for example, read data from a network, format a hard drive
or alter their behaviour based on some internal state.

One reason pure computations are nice is that they're easy to reason about, both
for humans and for algorithms. The prevalence of pure computations in Haskell is
a major reason why the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
is able to perform many heavy-duty optimisations: re-ordering, combining and
even throwing away large amounts of code, whilst preserving the original's
behaviour.

Many languages aren't so pure; any expression can make important
globally-visible, behaviour-determining effects at any time. It is important
that the implementation of such languages don't interfere with the order of
these effects, since in general effects don't commute (i.e. "X then Y" isn't
always equivalent to "Y then X"). Hence most languages can't make such sweeping
optimisations that GHC is able to.

Such implementations are *conservative*: since two expressions (e.g. procedure
calls) *could* be performing arbitrary effects which *could* be non-commuting,
such changes aren't attempted. By avoiding an optimisation completely, we
*definitely* avoid all problematic cases; but we unfortunately avoid all
non-problematic cases too.

It would be interesting if we could *query* such language implementations to ask
what the potential impurities are in some piece of code. For example, we might
ask a Python implementation what the impurities are in `x() + y()`, and it would
tell us that looking up `x` and `y` in the environment may not be pure; that
(attempting to) call their values as procedures may not be pure (e.g. if they're
not callable, an exception will be thrown, and the handler may be impure); that
if they are procedures, those procedures may not be pure; that looking up the
`__add__` method on the first result may not be pure and that calling such an
`__add__` method may not be pure. Whew!

This looks intimidating, but is mostly a matter of syntax: knowing which
language constructs can lead to effectful code being executed. In the above
example, we know that `+` is syntactic sugar for calling an `__add__` method,
and we know that looking up methods can be effectful. There's actually no need
to look through abstractions, e.g. if an expression contains a procedure call,
we can assume it's effectful without looking at the procedure.

What would such an ability buy us? For a typical codebase, probably not much.
However, we could combine this with *purity annotations*: if we annotate a
construct as being pure, it won't appear in our impurity queries. Pure
constructs might include e.g. simple arithmetic, but may also code with
*unobservable* effects; for example, a procedure which includes a memo table to
avoid recalculating outputs. We can't mark the *pieces* as pure, since they
involve mutating a persistent state, but the construct *as a whole* is pure.

*That* would be an interesting ability to have, since it lets us take
information about code out of a developer's head and put it into the machine.
One immediate benefit would be providing extra knowledge to those reading the
code at a later date: a reassurance that no matter what crazy, dynamic stuff is
going on inside a piece of code, that it shouldn't leak that nastiness to the
outside. Perhaps another would be to test, prove, disprove or infer such
annotations, to aid the developer in understanding their code.

Going back to the idea of optimisation, it would give those same reassurances to
the *machine* as well, allowing it to reason more deeply about the code,
replacing its "just in case" wariness with the confidence to make more invasive
changes, such as rearranging, supercompiling, etc.

One place that might benefit from such "impurity queries" and "pure from the
outside" annotations is build/packaging systems like Nix. Many Nix packages use
shell scripts extensively, although they're run in an isolated environment,
filesystem references are hard-coded and the results are put in a read-only
filesystem. For example, a script may look up various binaries in its `PATH`,
which is impure and subject to change; however, Nix may hard-code the `PATH`
during installation, that path itself may be read-only and be derived from a
content-based hash. Such measures turn the lookup into a pure construct, since
it will always find the same file. An optimiser could utilise this, e.g. to
inline the file, or to collapse pipelines based on their producer/consumer
behaviour.

As it stands, shells are far too dynamic for such optimisations to be sound; for
example, all sorts of dynamic hacks can be used to execute arbitrary code at
various points in a script's execution. If we could query for which ones affect
our code, we could take measures to disable them, annotate the result as pure,
and build a new optimised package out of the old one.
