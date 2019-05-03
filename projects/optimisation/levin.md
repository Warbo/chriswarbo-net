---
title: Levin Search
---
Levin Search is a specific type of enumeration, which has nice theoretical
properties for real-world problems.

In this example, the fitness of a particular location is shown by how light the
grey colour is. This is randomly generated when the page loads.

Each (x, y) point in the square is a solution, and our goal is to find the best
(lightest). Levin Search is based on Algorithmic Information Theory, which says
we should enumerate *programs* rather than coordinates. By converting the output
of each program into a coordinate, we are enumerating solutions in order of
(Kolmogorov) complexity, starting with the simplest.

For example, the (binary) coordinates `(101010101, 101010101)` are quite simple,
since a) the two coordinates are the same and b) they follow a simple
alternating pattern. We can generate these coordinates easily with a small
computer program, but if we enumerated coordinates in order
`(000000000, 000000000)`, `(000000000, 000000001)`, `(000000000, 000000010)`,
etc. it will take a long time to find it.

Unfortunately, due to the Halting Problem, we can't actually perform this
enumeration-of-program-outputs directly, since some programs may never
halt. Instead we use an approximation known as Levin search, where we test
programs up to a certain length and kill them after a certain amount of time,
then we increment the allowed length, double the allowed time and try
again. Levin Search will find the fittest, simplest solution, based on Levin's
complexity measure (program length + log2(runtime)).

Here our programs are written in a language known as BitBitJump, which is
well-suited to Levin search:

 - The semantics are simple to implement, since we just repeat one instruction
   over and over (in particular, there are no errors)
 - Programs are written in binary, which makes it easy to calculate the lengths
   and times
 - All binary sequences are valid programs, so we don't have to care about
   parsing

Programs in BitBitJump keep shuffling around the contents of an array of memory,
using addresses of a fixed size (the word size). Since this address size is
fixed, the BitBitJump machine can only use a fixed amount of memory. This
prevents it from being Turing Complete, but we can easily get around this by
incrementing the word size whenever our allowed length overflows the memory.

The programs themselves are simply numbers, which we count up from 0, 1, 2,
etc. We write out our current program's number in binary, and dump this into the
start of the BitBitJump machine's memory.

We enter each program into memory *backwards*. This is to counteract an artifact
of our writing system: we don't write leading zeroes in our numbers (eg. "100"
instead of "000100"), so the first bit of memory would always be "1" if we
entered programs in directly. By reversing the bits, the first bit can be "0" or
"1", and the leading zeroes get "filled back in" by the rest of the memory
(which is initialised to zeroes).

We read out the coordinates by taking the first 18 bits of memory, and assigning
all of the even bits to x and all of the odd bits to y. These 9 bit numbers are
then used as the coordinates, 0-511, since the search space is 512 pixels x 512
pixels.

Levin Search will always find a solution if one exists, since it will eventually
evaluate any solution for any number of steps. The first solution found will be
the "simplest" according to Levin's complexity measure, which in our case is the
length of the solution plus the logarithm of the number of steps it takes to
reach the desired result. Note that this is simply the current 'phase' (outer
loop).

<div id="levin_playfield" style="width: 512px; height: 512px;">
</div>

<form action="#" method="get">
 <div>
  Best fitness so far: <a href="#" id="levin_fitness_display"></a>
 </div>
 <div>
  Fittest program: <a href="#" id="levin_winner"></a>
 </div>
 <div>
  Current phase: <a href="#" id="levin_phase">0</a>
 </div>
 <div>
  Current machine size: <a href="#" id="levin_m">1</a>
 </div>
</form>

<script src="/js/jquery.js">
</script>

<script src="/js/jquery_svg.js">
</script>

<script src="/js/underscore.js">
</script>

<script src="/js/optimisation/levin.js">
</script>

<script src="/js/zot.js">
</script>

Click the square above to start the search. Since it's completely deterministic,
there's no point running more than one instance at a time. The "Best fitness so
far" value tells us what the search has found so far.

Whilst Levin Search is very simple and has nice theoretical properties, its
inability to learn makes it incredibly inefficient for all but the simplest
black-box problems.
