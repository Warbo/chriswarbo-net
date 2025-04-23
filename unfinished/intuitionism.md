---
title: Intuitionism in physics
---

Sabine Hossenfelder
[published a video recently](https://www.youtube.com/watch?v=oEWm3yPUosg)
discussing whether questions in Physics could benefit from an intuitionistic
approach to mathematics. It's an interesting idea, but it's rather subtle; and
tripping over those nuanced details can lead us astray (or give us the wrong
intuition, so to speak).

In particular, Sabine focused on the idea of
[real numbers](https://en.wikipedia.org/wiki/Real_number) and their "decimal
places", which is a useful model for introducing these ideas; but taking it too
far can result in some confusion. For example, something as simple as the number
⅓ may appear "infinitely complicated" when viewed as a never-ending decimal
0.333… On the other hand, using a different
[numerical base](https://en.wikipedia.org/wiki/Radix) can make it simple again
(e.g. in [dozenal](https://en.wikipedia.org/wiki/Duodecimal) it's just 0.4).

There *is* an important question lurking in the decimal places of real numbers,
but to understand it we can't "just" calculate or measure more digits.

### Intuitionism and constructivism ###

Full disclaimer, I'm not a trained mathematician, but I have done work in formal
systems, proof assistants and type theory; so I want to get across a high-level
picture, and try to clarify some of the points Sabine made.

I'll also note that the "intuitionist" maths that Sabine describes (where
mathematical objects 'exist in the mind', and are only 'real' once thought of)
is closely related to the idea of "constructive" maths (where mathematical
proofs must be direct, not double-negatives). I'll actually focus on the latter
(since I'm more familiar, and inclined to agree, with that perspective), which
is a bit of a philosophical bait-and-switch; but the practical results are
essentially the same (e.g. axiomatic systems which don't allow a general law of
excluded middle).

<!--
Since Constructive theories can have firstly they can have
richer structure than their classical counterparts (since the latter permit
extra axioms, leading to more equalities, which collapse those structures).
Secondly, since constructive theories don't force everything into a true/false
dichotomy, they let us naturally consider aspects like (un)provability,
(un)computability, etc.
-->

### Information, not decimal places ###

Sabine rightly points out a problem when considering a quantity's "number of
decimal places": that it can change dramatically. She gives logarithms and
exponentials as an example.

We can clarify the situation by taking a step back, and recalling what a
"decimal place" *actually is*: a way to narrow-down the possible range of a
number, into one of ten possible divisions (0... or 1... or 2... etc. up to
9...). Each decimal place we state narrows-down the range by another factor of
ten (although [the edges do overlap](/blog/2024-05-14-spigot.html)).

Grouping numbers into finite decimals and infinite decimals doesn't take us far
enough. We need to split up those infinite decimals further, into those which
are rational (whose digits repeat) and those which are irrational; then we split
those irrationals into the algebraic   A phrase like "infinite decimal" can encompass many things, from the
otherwise-simple ⅓ to something as inscrutable as
[Chaitin's constant](https://en.wikipedia.org/wiki/Chaitin%27s_constant); similar
loose, rational or "irrational"
https://en.wikipedia.org/wiki/Algorithmic_information_theory


### Algorithmic information theory ###

The finite/infinite distinction we should be making is not directly at the level
of decimal places, but more fundamentally at the amount of *information* present
in a number (or any other mathematical construct). Information is a *relative*
quantity, depending on how much it constrains an existing (or "prior")
probability distribution.

For computing the digits of a number, the relevant probability distribution is
over computations which give rise to those digits.
