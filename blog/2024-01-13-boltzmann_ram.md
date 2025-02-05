---
title: Boltzmann RAM
packages: [ 'mathml' ]
---

Simulating every computable universe (the constructivist/intuitionistic version
of Tegmark's level 4) is actually remarkably easy. Each of those universes can
be simulated by running some computer program (by definition of constrictivism),
so we can simulate all of them by "just" running every possible computer
program!

## A simple existence proof ##

There's actually a *single*, very simple, program which will simulate all
others, by [diagonalising](https://en.wikipedia.org/wiki/Diagonal_argument) over
the program lengths and running times:

 - Run all programs of length `1`{.unwrap pipe="num | math"} (i.e. programs
   `0` and `1`) for `1`{.unwrap pipe="num | math"} step each.
 - Re-run all programs of length `1`{.unwrap pipe="num | math"}, this time for
   `2`{.unwrap pipe="num | math"} steps. Then run programs of length
   `2`{.unwrap pipe="num | math"} (`00`, `01`, `10` and `11`) for
   `1`{.unwrap pipe="num | math"} step each.
 - Re-run programs of length `1`{.unwrap pipe="num | math"} for
   `3`{.unwrap pipe="num | math"} steps; programs of length
   `2`{.unwrap pipe="num | math"} for `2`{.unwrap pipe="num | math"} steps; and
   run all programs of length `3`{.unwrap pipe="num | math"} (`000`, `001`,
   `010`, etc.) for `1`{.unwrap pipe="num | math"} step each.
 - Keep going in this manner forever: each time re-running all previous programs
   for an extra step, then running all one-bit-longer programs for
   `1`{.unwrap pipe="num | math"} step.

If we wait long enough, this loop will eventually run *any* program for *any*
number of steps: including simulations of our universe being run for long enough
to reach the present day!

## A remarkable linear-time solution ##

The above algorithm is exponentially slow: each iteration runs twice as many
programs as the last (since there are
`num '2'; var 'l';`{.unwrap pipe="sh | mapply power | math"} programs of length
`l`{.unwrap pipe="var | math"}), so reaching step
`var 'n'; num '1'`{.unwrap pipe="sh | add | math"} of any program will take at
least twice as long as reaching step `n`{.unwrap pipe="var | math"}. That's
*a lot* of overhead, which will soon grind everything to a crawl, and makes this
whole endeavour seem far-fetched.

However,
[Schmidhuber suggests](http://people.idsia.ch/~juergen/computeruniverse.html) we
can avoid this in two ways:

 - Firstly we can 'spread out' the programs by writing them in a
   [prefix-free code](https://en.wikipedia.org/wiki/Prefix_code). This requires
   more bits to encode our programs, reducing the number of programs of each
   length; i.e. there are *fewer than*
   `num '2'; var 'l';`{.unwrap pipe="sh | mapply power | math"} programs of
   length `l`{.unwrap pipe="var | math"} when encoded in a prefix-free way.
 - Then, instead of re-running with an extra step each time, we instead re-run
   with *double* the number of steps, i.e. running for
   `1`{.unwrap pipe="num | math"} step, then `2`{.unwrap pipe="num | math"}
   steps, then `4`{.unwrap pipe="num | math"} steps, then
   `8`{.unwrap pipe="num | math"} steps, etc.

This doubling has an interesting effect: the original algorithm took twice as
long to reach step `var 'n'; num '1'`{.unwrap pipe="sh | add | math"} as to
reach step `n`{.unwrap pipe="var | math"}; yet this algorithm can reach step
`num '2'; var 'n'`{.unwrap pipe="sh | mult | math"}: that's *linear time*! In
other words, nothing will slow down over time: it's as if we were running each
program on a dedicated computer, yet we're actually running *all* programs on a
*single* machine!

```{pipe="sh > runtime.mml"}
{
  num '1'
  { num '2'; var 'l'; } | mapply 'power'
} | mapply 'divide'
```

The reason this works is that our changes (prefix-free encoding and
exponentially-distributed running times) cause the allocation of runtime between
programs to obey the [Kraft
inequality](https://en.wikipedia.org/wiki/Kraft%E2%80%93McMillan_inequality).
Each program gets a *constant fraction* of our runtime (equivalent to running
on a dedicated, albeit slower, machine). The downside is those fractions get
*very* small for larger programs: those of length `1`{.unwrap pipe="num | math"}
will get `num '1'; num '2';`{.unwrap pipe="sh | mapply divide | math"} the
runtime, programs of length `2`{.unwrap pipe="num | math"} will get
`num '1'; num '4';`{.unwrap pipe="sh | mapply divide | math"} of the runtime,
and in general programs of length `l`{.unwrap pipe="var | math"} will get
`cat runtime.mml`{.unwrap pipe="sh | math"} of the runtime. The use of a
prefix-free code ensures that there are few-enough programs of each length to
avoid these fractions adding up to more than `1`{.unwrap pipe="num | math"}.

```{pipe="sh > 10.mml"}
{
  {
    num '1'
    { num '2'; num '10'; } | mapply 'power'
  } | mapply 'divide'
  { num '1'; num '1024'; } | mapply 'divide'
} | mapply 'eq'
```

```{pipe="sh > sum.mml"}
{
  {
    num '1'
    num '2'
    num '4'
    num '8'
    num '16'
    num '32'
    num '64'
  } | add
  num '127'
} | mapply 'eq'
```

Finally, the *overall speed* of each program is only half of its allocated
runtime, so a program of (prefix-free encoded) length
`10`{.unwrap pipe="num | math"} will get
`cat 10.mml`{.unwrap pipe="sh | math"} of the runtime, but only run at
`num '1'; num '2048';`{.unwrap pipe="sh | mapply divide | math"} of its normal
speed. The reason is that we keep restarting everything. As an example, for a
program to reach step `100`{.unwrap pipe="num | math"} of its execution it must
be restarted many times, which wastes some of its allocated runtime: first it
wastes `1`{.unwrap pipe="num | math"} step, then is later restarted and run for
`2`{.unwrap pipe="num | math"} steps (making `3`{.unwrap pipe="num | math"}
wasted steps so far); then `4`{.unwrap pipe="num | math"} steps
(`7`{.unwrap pipe="num | math"} wasted so far); and so on. *Eventually* it will
be run for `128`{.unwrap pipe="num | math"} steps, which is enough for it to
reach step `100`{.unwrap pipe="num | math"} that we wanted. At that point it's
wasted `cat sum.mml`{.unwrap pipe="sh | math"} steps; and in general, when we
re-run a program for `S`{.unwrap pipe="var | math"} steps, it means we've
already wasted (one fewer than) `S`{.unwrap pipe="var | math"} steps on it so
far. Since we're thus spending (just less than) twice as many steps to reach
each point of a program's execution, this restarting is hence causing every
program to run at around half the speed it otherwise would.

Whilst these linear slowdowns of each program are exponential in their length;
they are nevertheless *constant over time*. Hence if we start this program
running, it will (gradually) spin-up a simulation of every possible universe
simultaneously (albeit most will be running at an extremely slow rate).

## Implications ##

The "simulation hypothesis" has recently entered popular culture as a serious
idea (building on previous, more fanciful incarnations like The Matrix). It asks
whether our Universe was *purposefully created* as a simulation, inside and of
some other "host" Universe (perhaps to investigate historical or counterfactual
scenarios). That's a nice philosophical idea, akin to thought experiments like
the classic [brain in a jar](https://en.wikipedia.org/wiki/Brain_in_a_vat), but
hence just as self-defeating: they are predicated on our inability to discern
the difference between their postulated worlds, so they therefore make no
discernable difference to anything either way!

In contrast, any simulations run by the above algorithms are *not* purposeful:
if run for long enough, they *will* simulate our Universe, and countless others;
not because we were specially chosen, but precisely because we are *not*
fundamentally different from any other computer program! Since there's no
postulated "host", to [select and manipulate the
rules](https://en.wikipedia.org/wiki/Omphalos_hypothesis), we can treat this
setup in a scientific way; more akin to a
[Boltzmann brain](https://en.wikipedia.org/wiki/Boltzmann_brain).

The idea of a Boltzmann brain is that random movements in some collection of
matter, like a gas cloud, may *occasionally* bring their constituent parts
together in a way which is
[capable of thought](https://en.wikipedia.org/wiki/Cogito,_ergo_sum). The usual
argument against this as an explanation for our observations is that
smaller arrangements of matter are exponentially more likely to occur by chance
than larger arrangements. Hence, if we were such a Boltzmann brain, we would
expect to see very little structure or order around us; yet we observe a vast
Universe filled with structure.

(Of course, such "observations" might have simply appeared, by pure coincidence,
as "memories" inside a randomly assembled Boltzmann brain which *thinks* it's
observed a structured Universe; and this line of reasoning can never be
completely disproved. However, the further we travel down that road, the closer
we get to those unfalsifiable-by-definition ideas like the simulation hypothesis
and the brain in a jar; and hence the easier it becomes to dismiss that whole
direction as empirically irrelevant!)

### Boltzmann RAM ###

Rather than applying the Boltzmann brain idea to the structure that
*constitutes* us, we can instead apply it to a structure that *computes* us; for
example, by flipping random bits on a Turing machine tape. Again, it's unlikely
that an algorithm will arise by chance for simulating precisely us, or our
Universe, due to how complex it is to describe: it's exponentially more likely
for smaller, simpler programs to arise.

However, we've seen above that there are small, simple programs that *do*
simulate our Universe (albeit not *precisely*, since they also run *every other*
Universe alongside). Such programs may well arise by chance, given a
reasonable number of bit flips. In that case, we need to ask how likely our
observations about the Universe would be, if there were so many simulations
running at once. Since "disorder" takes more bits to encode, and the above
algorithms allocate less runtime to longer programs, we would expect most minds
to observe an ordered, structured Universe (so long as there's *enough*
complexity for such minds to arise at all)!

## Conclusion ##

Note that this hypothesis does not postulate some 'higher level' Universe that
runs the computation of ours; it could instead be the case that such computation
is just the underlying nature of reality. As an analogy, General Relativity
assumes curved spacetime is the underlying nature of reality, that's why Earth
orbits the Sun, and the motion can be calculated using tensor algebra. Yet that
*does not* imply there must be some intelligent beings performing those
calculations in order for the motion to occur: it just happens, all on its own.
Likewise, it may be the case that computations "just happen", and we can use
complexity arguments to make falsifiable predictions about what's more or less
likely to happen.

To his credit, Schmidhuber followed this line of reasoning to arrive at a
prediction that large-scale quantum computation will not be possible: precisely
because the computational effort required to describe that behaviour would make
Universes containing them very unlikely, compared to Universes which constrain
quantum effects to more easily-calculated realms (perhaps taking shortcuts to
arrive at classical solutions on the macro-scale). Whilst I doubt that will be
the case, it's certainly a novel perspective on cosmology; and yet another
justification for trying to scale up our quantum computers!
