---
title: Finite-choice logic programming
---

Found via [this Lobste.rs comment](https://lobste.rs/s/9anym5/what_was_best_research_paper_you_read_2024#c_cxptwq)

Original implementation biases the search towards immediate consequences, i.e.
making "choices" which only have a single option. This makes it much more
efficient than e.g. selecting which choice to make uniformly at random, but they
point out that it may fail to terminate.

How about we bias the search using a Levin-style exponential distribution, e.g.
make a tree like:

  +-- A1
  |
+-+-- A2
  |
  +-- A3
  |
  |  +-- B1
  |  |
  +--+-- B2
     |
     |  +-- C1
     |  |
     +--+
        |
        +--...

Where Ai have the same number of options as each other, Bi have the same number
of options as each other, and so on; and Bi have more options than Ai, Ci have
more options than Bi, etc.

To select which choice to make, we start at the root and pick a child uniformly
at random: if we hit a child, that's the choice we make next; otherwise we
recurse. In the above example, A1, A2 and A3 will each be chosen 1/4 of the
time; B1 and B2 will each be chosen 1/3 * 1/4 = 1/12 of the time; C1 will be
chosen 1/12 * 1/2 = 1/24 of the time; and the remaining 1/24 of the time we'll
recurse down to something in the '...' branch.

This may be a nice framework for a 'Gen a' implementation, useful for property
checking; and indeed for generating/enumerating expressions. Would be useful to
compare it to equation graphs, like egglog, etc. to see if this constraint
framework can be used/extended to account for efficient handling of equational
relations (e.g. via hash consing, etc.)?
