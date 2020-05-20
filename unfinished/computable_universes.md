Simulating every computable universe (the constructivist/intuitionistic version
of Tegmark's level 4) is actually remarkably easy. Each of those universes can
be simulated by running some computer program, so we can simulate all of them by
running every possible computer program. We can schedule them on a single
machine by diagonalising over the program lengths and running times: run
programs '0' and '1' for 1 step each; then run programs '00', '01', '10' and
'11' for 1 step each and programs '0' and '1' for 2 steps each; then run '000',
'001', '010', etc. for 1 step each, then '00', '01', '10' and '11' for 2 steps
each, then '0' and '1' for 3 steps each; and so on. For any finite program P and
step count S (such as a simulation of our universe and enough steps for it to
reach the present day), this simple algorithm will eventually run P for S steps.

One problem with that algorithm is that it's exponentially slow: each "phase"
doubles the number of programs that we try, and runs everything for longer, so
after we've finished running a particular program we'll have to wait more than
twice as long before it comes round again.

Schmidhuber suggests compensating for this in two ways (
http://people.idsia.ch/~juergen/computeruniverse.html ): first we double the
number of steps we execute each time, i.e. running for 1 step, then 2 steps,
then 4 steps, then 8 steps; secondly we 'spread out' the programs by writing
them in a prefix-free code. Remarkably, this lets us run every possible computer
program in linear time, all at once! Programs of length 1 will always run at 1/2
speed, programs of length 2 will always run at 1/4 speed, and in general
programs of length N will always run at 2^-N speed; nothing will slow down over
time. If start this program running, it will simulate every possible universe
simultaneously (although most will be running at an extremely slow rate).

It might be the case that our universe is being simulated by such a simple
program. We don't need to postulate some 'higher level' either, that could just
be the underlying nature of reality (in the same way that, for example, general
relativity assumes curved spacetime is the underlying nature of reality; i.e. we
don't assume Earth's orbit of the Sun requires some higher-level aliens to
calculate the tensor algebra for it to happen; it just happens).
