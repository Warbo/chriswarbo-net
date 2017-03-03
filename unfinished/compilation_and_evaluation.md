---
title: Compilation, Interpretation and Optimisation
---

Interpreters do not look at a program's expression until it's to be executed, at
which point they pass it through a fixed, deterministic program to reduce it
into a result, e.g. by looking up names, substituting arguments into functions,
matching patterns, etc.

Just-in-time compilers are like interpreters, but the way they evaluate the
expression is potentially unpredictable, as they may have multiple strategies
with different resource usage. Interpretation has a low up-front cost but
potentially costly operations; "JITting" may decrease the cost of
operations (e.g. by specialising operations to avoid dynamic dispatch) but
has a larger up-front cost.

Ahead-of-time compilers look at a program's expression before it is executed,
and may transform it in potentially-unpredictable ways (e.g. as a result of
optimisations and their interactions). They can store this transformed
expression, an 'executable', for running later, potentially many times. This
amortises the up-front cost of optimisations.

Hutter search is an evaluation strategy which concurrently runs a program and
a few search procedures. If the searchers discover an equivalent, faster
alternative expression, the interpreter is aborted and the evaluation is
restarted with the new expression. This seems related to JIT, e.g. we can keep a
priority queue of search states for various expressions; to evaluate an
expression we use Hutter search, restoring the proof search from the priority
queue if found. This concentrates the search time on long-running sections of
code.

Partial application specialises a function to a particular argument value.

Compilation is partial application of an interpreter to a program (the first
Futamura projection).

Partially applying a partial applier to an interpreter produces a compiler
(second Futamura projection).

Partially applying a partial applier to a partial applier produces an
interpreter-to-compiler specialiser.

Supercompilation is an optimisation which performs as much partial application
as possible before execution. The idea of supercompilation is to pre-calculate
everything which *doesn't* depend on dynamic information, and to make any
dynamic decisions as *early* as possible, so that the resulting program
becomes a linear sequence of instructions. NOTE: Is this right? Perhaps it's
better to *combine* dynamic decisions as much as possible, so that everything
in-between is linear? Read some official definition for clarification...

Superoptimisation is an optimisation which replaces expressions with faster
equivalents, found using search.

Theory exploration searches for equivalences, which may be optimisations.

Logic programming searches for matches to a query.

What if we supercompile a logic program? Could we e.g. eliminate branches which
are statically-known to be fruitless? Could we fuse and rearrange search paths,
to reduce the search space?

Could we optimise the search strategy, maybe using something like theory
exploration (plugging in random values) to prioritise paths?

Could logic programming provide an approach to theory exploration which is more
general, i.e. finding equivalences, conditional equivalences, properties, etc.?
Must be careful not to become too general; maybe stick to a goal like "useful
for self-improvement"?
