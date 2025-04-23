---
title: Finite-choice logic programming
packages: [ 'mathml' ]
---

I recently came across finite-choice logic programming via
[this Lobste.rs comment](https://lobste.rs/s/9anym5/what_was_best_research_paper_you_read_2024#c_cxptwq).
It's an approach to logic-programming distinct from the usual depth/breath-first
search algorithms, where we instead bias towards immediate consequences, i.e. by
starting with those "choices" that only have a single option. That's much more
efficient than e.g. selecting choice points uniformly at random (which can send
us down long branches); but they point out that it may fail to terminate.

I like the perspective it gives on the situation, but I wonder whether it would
be useful to weight the choices exponentially (similar to Levin search), rather
than *always* taking the immediate consequences. For example, we could arrange
the choices in a tree like:

```
  ┌── A1
  │
──┼── A2
  │
  ├── A3
  │
  │  ┌── B1
  │  │
  └──┼── B2
     │
     │  ┌── C1
     │  │
     └──┤
        │
        └──…
```

Where `Ai` have the same number of options as each other, `Bi` have the same
number of options as each other, and so on; and `Bi` have more options than
`Ai`, `Ci` have more options than `Bi`, etc.

```{.unwrap pipe="sh > As.mathml; pandoc -t json"}
frac 1 4
```

```{.unwrap pipe="sh > Bs.mathml; pandoc -t json"}
frac 1 12
```

```{.unwrap pipe="sh > Cs.mathml; pandoc -t json"}
frac 1 24
```

```{.unwrap pipe="sh > EqBs.mathml; pandoc -t json"}
{
  {
    frac 1 3
    cat As.mathml
  } | mult
  cat Bs.mathml
} | mapply eq
```

```{.unwrap pipe="sh > EqCs.mathml; pandoc -t json"}
{
  {
    cat Bs.mathml
    frac 1 2
  } | mult
  cat Cs.mathml
} | mapply eq
```

To select which choice to make, we start at the root and pick a child uniformly
at random: if we hit a leaf, that's the choice we make next; otherwise we
recurse. In the above example, `A1`, `A2` and `A3` will each be chosen
``{.unwrap pipe="math < As.mathml"} of the time; `B1` and `B2` will each be
chosen ``{.unwrap pipe="math < EqBs.mathml"} of the time; `C1` will be
chosen ``{.unwrap pipe="math < EqCs.mathml"} of the time; and the remaining
``{.unwrap pipe="math < Cs.mathml"} of the time we'll recurse down to something
in the '…' branch.

This may be a nice framework for a `Gen a` implementation, useful for property
checking; and indeed for generating/enumerating expressions. Would be useful to
compare it to equation graphs, like egglog, etc. to see if this constraint
framework can be used/extended to account for efficient handling of equational
relations (e.g. via hash consing, etc.)?
