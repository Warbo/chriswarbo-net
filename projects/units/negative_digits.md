---
title: Negative Digits
---

$$\def\ngtv#1{{\overline{#1}}}$$

This page describes an elegant method for taking one number away from another,
which I claim is simpler than the traditional "long subtraction" (that I was
taught in school), and it requires no "carrying". It uses the idea of
[negative digits](https://en.wikipedia.org/wiki/Signed-digit_representation),
which also (in case you're interested) happen to be a nice generalisation of
the way we represent numbers!

## Notation ##

"Taking away" is often represented as *subtraction*, written with a (binary)
*minus sign*, like this:

$$102843 - 67592$$

I find [subtraction to be overly-complicated](subtraction.html), and instead
prefer to *add a negative*. That would usually be written with a (unary) minus
sign, like this:

$$102843 + (-67592)$$

Mixing between two different "minus signs" is awkward, so I prefer to write
negatives by giving them an over-bar, which looks like this:

$$102843 + \ngtv{67592}$$

This is the notation I'll be using below. If you prefer, you can use subtraction
and minus signs instead; although you may need to space-out your writing to keep
things nicely aligned!

## The Setup ##

We'll use the example above, and calculate $102843 + \ngtv{67592}$. We'll start
by writing one number above the other (either way around), with their digits
aligned to the right. As per tradition, we put the + on the bottom left and
draw a horizontal line below (our result will be written below):

$$
\begin{array}[t]{r}
         102843 \\
+ \ \ngtv{67592} \\ \hline
\end{array}
$$

Note: If you're using subtraction, then the order matters! You need to write the
first number on top and the second on the bottom.

When we write down fractions, we usually try to normalise (or "simplify") them
by removing common factors; e.g. we would normalise $\frac{6}{8}$ to
$\frac{3}{4}$; we would normalise $\frac{11}{99}$ to $\frac{1}{9}$, and so on.
However, these un-normalised forms can make life easier during calculation it can
 - We don't tend to mis

I find it *especially* pleasing when notations and concepts *coincide*: that is,
when some notation *appears* to have two conflicting uses, but turn out to both
mean the same thing!

As an example, bar notation is already used for

e.g. a number like $12\ngtv{3}4$ meaning
$1 \times 10^3 + 2 \times 10^2 + \ngtv{3} \times 10^1 + 4 \times 10^0$ (the
number $1174$, with non-negative digits). With bar notation, the digits ($1$,
$2$, $\ngtv{3}$ and $4$) will always match the coefficients. We don't get that
with minus signs (where the digit $\ngtv{3}$ would give a coefficient $-3$).

There is also a more subtle coincidence between bar notation for negative digits
and negative numbers: if *all* of a number's digits are negative, that number
will be negative. As long as we "join together" the bars of neighbouring
digits, this coincides *exactly* with our negative number notation. For example,
an expression like $\ngtv{9876}$ can be interpreted *both* as the negative of $9876$
*and* as a number written with negative digits (representing $\ngtv{9} \times
10^3 + \ngtv{8} \times 10^2 + \ngtv{7} \times 10^1 + \ngtv{6} \times 10^0$);
they both refer to the same quantity!

TODO: Make page on separating place from value
