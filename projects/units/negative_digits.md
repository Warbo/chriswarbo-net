---
title: Negative Digits
---

$$\def\ngtv#1{{\overline{#1}}}$$

This page describes an elegant method for taking one number away from another,
which I claim is simpler than the traditional "long subtraction" (that I was
taught in school), and it requires no "carrying". It uses the idea of
[negative digits](https://en.wikipedia.org/wiki/Signed-digit_representation),
which also (in case you're interested) happens to be a nice generalisation of
the way we represent numbers!

## Notation ##

"Taking away" is often represented as *subtraction*, written with a *minus
sign*, like this (these are just arbitrary numbers, picked at random):

$$102843 - 67592$$

This is sometimes called a "binary minus sign", since it acts on *two*
numbers, written to its left and right.

I find [subtraction to be overly-complicated](subtraction.html), and instead
prefer to *add a negative*. That would usually be written like this:

$$102843 + (-67592)$$

This time we have a "unary minus sign", since it's only acting on *one* number,
written to its right (if you prefer, you can think of the unary minus sign as
acting like a binary minus sign whose left-hand-side is always $0$).

Mixing between these two different "minus signs" is awkward, so I prefer to
write negatives by giving them an over-bar, which looks like this:

$$102843 + \ngtv{67592}$$

This is the notation I'll be using below. If you prefer, you can use subtraction
and/or minus signs instead; although you may need to space-out your writing to
keep things nicely aligned!

## The Setup ##

We'll use the example above, and calculate $102843 + \ngtv{67592}$. We'll start
by writing one number above the other, like in normal long-addition; since we're
adding, it doesn't matter which goes above/below. We align their digits to the
right, so the "ones" line up, the "tens" line up, etc. (I've also inserted a
leading $0$ as "padding"). As per tradition, we write + on the bottom left and
draw a horizontal line underneath (our result will be written below this line):

$$
\begin{array}[t]{r}
         102843 \\
+  \ngtv{067592} \\ \hline
\end{array}
$$

Note: If you're using subtraction, then the order matters! You need to write the
number from the left on top and the one from the right on the bottom.

### Reading $\ngtv{67592}$ ###

We're familiar with how positive numbers are made from their positive digits,
by treating each digit as a multiple of the base raised to decreasing powers
(in decimal, the base is ten); for example:

$$
142 = 1 \times 10^2 + 4 \times 10^1 + 2 \times 10^0 \\
    = 100 + 40 + 2
$$

(Note that I wrote "ten" rather "10", since the latter always means "one times
the base", which therefore *depends on the base!*)

In exactly the same way, we can interpret negative numbers as having *negative
digits*, e.g.

$$
\ngtv{142} &= \ngtv{1} \times 10^2 &+ \ngtv{4} \times 10^1 &+ \ngtv{2} \times 10^0
           &= \ngtv{100}           &+ \ngtv{40}            &+ \ngtv{2}
$$

In other words, we can read a negative number like $\ngtv{67592}$ in two ways:
as the negative of $67592$ (which itself is a sum of multiples of 10), i.e.

$$
\ngtv{67592} &= \ngtv{1} \times 67592 \\
             &= \ngtv{1} \times (60000 + 7000 + 500 + 90 + 2)
$$

*Alteratively* we can read $\ngtv{67592}$ as a sum of *negative* multiples of
10, e.g.

$$
\ngtv{67592} = \ngtv{60000} + \ngtv{7000} + \ngtv{500} + \ngtv{90} + \ngtv{2}
$$

## Calculating ##

Back to our arihmetic: we proceed just as in long-addition, adding the
right-most digits (the "ones"), and progressing to the left. Reading negative
*numbers* as a sequence of negative *digits* lets us do two things: firstly, we
will apply the usual digit-by-digit algorithm *regardless* of whether they're
positive or negative; and secondly we'll allow our answer to use both positive
*and* negative digits if needed.

Let's start with the right-most column: $3 + \ngtv{2}$. Their sum is $1$, so
that's the right-most digit of our answer:

$$
\begin{array}[t]{r}
          102843  \\
+ \ \ngtv{067592} \\ \hline
               1
\end{array}
$$

Moving left one column, we need to calculate $4 + \ngtv{9}$, which equals
$\ngtv{5}$, so that's the next digit of our answer. Note that the usual
long-subtraction algorithm would give up here, and ask us to "carry a one" from
the third column!

$$
\begin{array}[t]{r}
          102843  \\
+ \ \ngtv{067592} \\ \hline
       \ngtv{5}1
\end{array}
$$

Our result now contains positive and negative digits, but that's fine. We can
proceed through the rest: $8 + \ngtv{5} = 3$, $2 + \ngtv{7} = \ngtv{5}$,
$0 + \ngtv{6} = \ngtv{6}$, $1 + \ngtv{0} = 1$. The overall result is:

$$
\begin{array}[t]{r}
         102843  \\
+ \ \ngtv{67592} \\ \hline
      3\ngtv{5}1
\end{array}
$$

## Digital Independence ##

We performed the above caculation by working right-to-left through the digits.
That's *required* when doing "traditional" long-addition or -subtraction, and
it helps "line up" the columns if we don't want to left-pad with zeros.
However, introducing negative digits removes this constraint: we can add up
the columns in *any* order, since there are no dependencies; i.e. we never
need to "carry" a number from one column to another.

To see this, we can look at the extreme cases: the digit with smallest magnitude
added to that with largest magnitude. Those cases are $0 + \ngtv{9} = \ngtv{9}$
and $\ngtv{0} + 9 = 9$. All other combinations are bounded by these extremes, so
we never need more than one digit (possibly negative to represent the answer.
For example, no pair of digits will ever add up to $10$.

## Normalising ##

---

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
