---
title: Using Theory Exploration
---

## Intro to Theory Exploration

My current research involves automated [theory exploration]() in
[Haskell](http://www.haskell.org). In short, we select a bunch of Haskell
functions as the input to the theory exploration tool, and it outputs a set of
equations which those functions satisfy.

The simplest way to use theory exploration is via [QuickSpec](), since it can be
used much like any other static analysis or testing tool on Hackage. QuickSpec
takes a *signature* (a set of functions, constants and variable names) and
outputs a set of equations relating the signature's elements, which it finds
using a combination of brute-force term generation and [randomised property
checking]().

QuickSpec was made by a group at [Chalmers
University](http://www.chalmers.se/en) who I had the pleasure of visiting
earlier this year. My own work is focused on using [machine learning]() to find
alternative ways to generate expressions (ie. not brute force), instantiate
variables (ie. not completely at random) and judge the merits of the output
(ie. not simple logical entailment). I'm still at the early stages, although I'm
close to releasing something which runs.

A colleague has asked me on a few occasions whether theory exploration would
help him with some particular problem, eg. finding lemmas to help prove some
statement in [Peano arithmetic](). My answer is usually that yes, it will find
properties such as X, Y, Z, but a) setting up a run may not be worth the time
for such simple systems and b) the lemmas X, Y and Z that we discussed as
examples may be all that's needed!

This made two things clear to me. Firstly, I've never actually made use of
theory exploration, other than running examples. Secondly, its main application
area seems to be in bespoke logical systems, where the behaviour may not yet be
known and there is a lot of the low-hanging fruit up for grabs, rather than in
established systems like Peano arithmetic which have already been scrutinised by
many experts. The latter reenforces my intuition that it will have major utility
in software development, where armies of programmers are churning out vast
libraries of bespoke functions without a full understanding of their behaviours
and relationships; rather than Mathematics, where systems of interest are
smaller and low-hanging fruit is often long gone.
