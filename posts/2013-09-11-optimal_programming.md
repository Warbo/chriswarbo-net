---
title: Optimal Programming
---
I'm a programmer, and as such I'm always looking for ways to automate and abstract away repetitive and time-consuming tasks. The most repetitive and time-consuming thing I do is program, so why not try to automate it?

There have been many approaches to automating programming over the years. Most, like inductive programming and [page=":cedi:type=misc:id=40"]Genetic Algorithms[/page], try to increase the power of toy systems. A few, like [page=":cedi:type=misc:id=46"]Levin Search[/page], try to increase the efficiency of theoretical systems.

I prefer the top-down theoretical approach, since 'heuristics come and go, but theorems are forever', but these algorithms tend to perform terribly on small problems, despite being optimal for large problems. The trouble is, all of the problems we care about are 'small' according to these algorithms.

As I've written previously, Levin Search is the optimal way to automate programming *when the only information we have is whether each guess is right or wrong*. Levin Search is only linearly slower than any other method, such as Genetic Algorithms, but the factor is huge.

The reason is that the limited-information scenario where Levin Search shines is actually a worst-case scenario; we usually have much more information available to exploit than just yes/no replies. For example, for a Genetic Algorithm to work well it needs enough bits in its fitness function to make the expected variance of the population high enough to learn from.

Is there a theoretically-best way to handle information? Yes, Solomonoff Induction, although that is incomputable. So on one hand we can make no use of information and sample from a universal prior, ie. we have Levin Search. On the other hand we can make maximum usage of information, which is Solomonoff Induction. What we would really like is a smooth curve between the two, and I think this can be parameterised by the ratio of evaluation cost / computational cost:

 - If the ratio is zero, there's no point wasting precious computation working out which candidate to evaluate next; just rattle through them in a fixed order. In other words, do Levin Search.
 - If the ratio is large then computation must be cheap in comparison to evaluating a candidate, so there's no point evaluating the next candidate until we've used a vast amount of computation to make the best selection we can. In the limit this gives Solomonoff Induction, since the incomputability is 'cancelled out' by the zero cost of computation, ie. we can use our infinite computing resources to solve the Halting Problem and calculate Solomonoff's prior.

It's interesting to wonder what the curve between these would look like. I might play around with some equations to see if I can make a pretty picture :)