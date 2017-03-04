---
title: Listing Things
---

In [From Euclid to Cantor](
http://www.flyingcoloursmaths.co.uk/from-euclid-to-cantor),
[Colin Beveridge](http://www.flyingcoloursmaths.co.uk) summarises and compares
two well-known proofs: Euclid's proof of the infinitude of primes, and Cantor's
proof that the cardinality of the reals is larger than that of the naturals. He
notes how they both rely on attempting to construct an exhaustive list of some
type of object (prime numbers and real numbers, respectively), then using that
list to construct a new object which doesn't appear in the list, hence proving
that it cannot be exhaustive.

Since I'm of the opinion that [the reals get far more attention than they deserve](
https://en.wikiquote.org/wiki/Leopold_Kronecker), I spotted that Colin jumps
straight from the rationals to the reals, pointing out that the latter includes
irrational numbers like $\sqrt{3}$, $\pi$ and $e$. This seems to be a common
habit among mathematicians, so I thought I'd take a look at a set of numbers
"in between" (in a sub/superset sense) the rationals and the reals.

One candidate we might look at is the [algebraic numbers](
https://en.wikipedia.org/wiki/Algebraic_number), since they include roots of
polynomials like $\sqrt{3}$; however, $\pi$ and $e$ are *not* algebraic
(non-algebraic numbers are called [transcendental](
https://en.wikipedia.org/wiki/Transcendental_number)).

Instead we'll look at the set of [computable numbers](
https://en.wikipedia.org/wiki/Computable_number), which I think are underrated.
All of these examples, along with ([most](
https://en.wikipedia.org/wiki/Chaitin%27s_constant)) 'interesting' numbers,
are computable.

It turns out we can list computable numbers very easily, by reading the output
of every computer program. There are many equivalent ways we can do this, here's
a simple one.

 - Choose some [universal Turing machine](
   https://en.wikipedia.org/wiki/Turing_machine#Universal_Turing_machines). To
   make things easy we'll pick one without a halt state, and we'll make it
   [monotone](http://people.idsia.ch/~juergen/toesv2/node6.html): this means we
   give it an extra tape, which only moves in one direction. This will be our
   "output".
 - For the $n$th number in the list (starting from the 0th), write $n$ on to the
   machine's work tape. Use the number of symbols as the base, e.g. if the
   machine uses binary, write the number in binary. Start with the
   least-significant digit and end with the most-significant (what would happen
   if we wrote it with the most-significant digit first?)
 - Run the machine; it will keep going forever, since there's no "halt" state.
 - Read the contents of the output tape as a number. One simple way is to read
   two sequences of digits: the first comes from every other cell on the tape
   (1st, 3rd, 5th, etc.); the second come from the cells in between (2nd, 4th,
   6th, etc.). We turn these into a number by writing a [radix point](
   https://en.wikipedia.org/wiki/Radix_point) and having the first sequence go
   off to the left, and the second go off to the right.

For example, if the output begins [0,1,2,3,4,5,6,7,…], we would get a number
like …6420.1357…

Some programs will never output any bits; some will output a finite number of
bits then switch to not outputting anything; others will keep outputting bits
forever. Since there are [algorithms](
https://en.wikipedia.org/wiki/Spigot_algorithm) which spit out the bits/digits
of numbers like $\pi$ and $e$ forever, we will eventually run a program
implementing such an algorithm, and hence those numbers will appear in our list,
despite having a never-ending decimal.

Since we can list all computable numbers, they have the same cardinality as the
natural numbers. This shows that [almost all](
https://en.wikipedia.org/wiki/Almost_all) real numbers are *uncomputable*, and
since [the universe is computable](
https://en.wikipedia.org/wiki/Church_Turing_Thesis), almost all real numbers
aren't actually "real" (and hence my opinions of them ;) )
