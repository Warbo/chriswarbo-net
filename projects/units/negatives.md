---
title: Negatives
parseArgs: []
renderArgs: [ "--mathjax" ]
extra_head:
  <script src="../../js/mathjax/tex-mml-chtml.js"></script>
  <script src="../../js/mathjax/polyfill.min.js"></script>
---

"Negatives" (like [negative
numbers](https://en.wikipedia.org/wiki/Negative_number)) are a great idea in
many situations, but they can be tricky to learn and are a common source of
mistakes. Here are some less-conventional ways to make them easier to work with,
and hopefully easier to understand.

Caveat: I'm talking about normal, everyday maths here; nothing exotic, like
finite fields or whatever!

NOTE: I will be using
["over-bar" notation for negation](negative_bar_notation.html) (like $\ngtv{5}$)

## You Ain't Gonna Need It! ##

As always, we should avoid unnecessary complications where possible. Situations
which are easily modelled without negative numbers shouldn't have them
shoe-horned in "for the sake of it", or "by default" (the same way we shouldn't
default to fractions, complex numbers, surreal numbers, etc. when they're
not needed).

For example: Alice has 3 apples, Bob has 4 apples; Alice gives 2 apples to
Bob, how many apples does Bob then have? Bob is *gaining* apples, so negatives
aren't needed. Of course, when a situation *does* make sense with negatives we
*should* use them: e.g. extending the example to ask how many apples *Alice*
ends up with, which we can model by adding $\ngtv{2}$ to her $3$. We could
instead use subtraction, but that *also* requires negatives (in case of
"underflow"), so [I prefer the former](subtraction.html).

Whilst this may seem obvious, note that many programming languages get it wrong:
e.g. having their `length` functions a (signed) *integer*, rather than an
(unsigned) *natural number*; forcing callers to account for the negative cases.

## Defining Negatives with Plus and Zero ##

We can define the "meaning" of negatives using the following property:

(@negative) $$x + \ngtv{x} \equiv 0$$

In other words, the negative of something is whatever we can *add* to that thing
to get a result of $0$.

Note that this definition *does not* involve subtraction, since [I consider
subtraction to be more trouble than it's worth](subtraction.html).

### The Negative of $0$ ###

We know that adding $0$ to anything will leave that thing unchanged ($0$ is
called the "identity" value for addition), i.e.

(@zero) $$x + 0 = x$$

This tells us that, as a special case, $0 + 0 = 0$ (in the case that $x = 0$).
Since that sum equals $0$, it fits our above definition of a negative. Hence the
negative of $0$ is itself $0$:

(@zero-bar) $$\ngtv{0} = 0$$

### "Double Negatives" (Negatives of Negatives) ###

If we take the negative of something that's already a negative, we get back the
original thing:

(@double-negative) $$\ngtv{\ngtv{x}} = x$$

<details class="odd">
 <summary>Proof</summary>

We can add $0$ to a "double negative" without changing it (as per equation
(@zero)):

$$\ngtv{\ngtv{x}} = \ngtv{\ngtv{x}} + 0$$

We can replace $0$ with the sum of a value and its negative (as per equation
(@negative)). Let's replace the $0$ with the sum of $x$ and its negative
$\ngtv{x}$:

$$\ngtv{\ngtv{x}} = \ngtv{\ngtv{x}} + x + \ngtv{x}$$

Let's rearrange this sum, and introduce parentheses to make the next step
clearer:

$$\ngtv{\ngtv{x}} = x + (\ngtv{x} + \ngtv{\ngtv{x}})$$

The expression in the parentheses is the sum of a value ($\ngtv{x}$) and its
negative ($\ngtv{\ngtv{x}}$), which is $0$ (according to equation (@negative)):

$$\ngtv{\ngtv{x}} = x + 0$$

Finally, we can get rid of the addition since it doesn't change the value (as
per equation (@zero)):

$$\ngtv{\ngtv{x}} = x$$

QED

</details>

Hence such "double negatives" will "cancel out".

### Expanding, Contracting and Shifting Negatives ###

When negatives are multiplied, we can "expand" the bar to cover the whole
multiplication:

(@expand) $$\ngtv{x}y = \ngtv{xy}$$

<details class="odd">
  <summary>Proof</summary>

Adding $0$ to our multiplication won't change its value (equation (@zero)):

$$\ngtv{x}y = \ngtv{x}y + 0$$

We can replace that $0$ with the sum of a value ($xy$) and its negative
($\ngtv{xy}$) as per equation (@negative):

$$\ngtv{x}y = \ngtv{x}y + xy + \ngtv{xy}$$

The first two terms have a common factor of $y$, which we can factor out:

$$\ngtv{x}y = (\ngtv{x} + x)y + \ngtv{xy}$$

Those parentheses contain the sum of a value ($x$) and its negation
($\ngtv{x}$), which is equal to $0$ (equation (@negative)):

$$\ngtv{x}y = 0y + \ngtv{xy}$$

Multiplying by $0$ always gives $0$:

$$\ngtv{x}y = 0 + \ngtv{xy}$$

Finally we can remove the addition (as per equation (@zero)):

$$\ngtv{x}y = \ngtv{xy}$$

QED

</details>

We can use the same approach for the second value (except we'll get a common
factor of $x$ in the proof), to show that:

(@expand-right) $$x\ngtv{y} = \ngtv{xy}$$

These equations also tell us we can "contract" a bar from an entire
multiplication down to just one of its factors.

In combination, these let us "shift" bars from one factor to another:

(@shift) $$\ngtv{x}y = x\ngtv{y}$$

### Multiplying Negatives ###

Consider the multiplication of *two* negatives: we can "expand" one of the bars
to cover the whole product (as per equation (@expand)):

$$\ngtv{x}\ngtv{y} = \ngtv{x\ngtv{y}}$$

From equation (@expand-right), we can replace $x\ngtv{y}$ with $\ngtv{xy}$:

$$\ngtv{x}\ngtv{y} = \ngtv{\ngtv{xy}}$$

This is a double negative, which cancels out (via equation (@double-negative)):

$$\ngtv{x}\ngtv{y} = xy$$

In this way, multiplications can always be simplified to contain either one
negative, or no negatives at all.

### The Role of $\ngtv{1}$ ###

We can always multiply values by $1$ without changing them, i.e. $x = 1x$ (we
say $1$ is the "identity" value for multiplication). This combines nicely with
the expansion, contraction and shifting of negatives:

$$\ngtv{x} = \ngtv{1x} = \ngtv{1}x$$

This lets us think about a value's "direction" ($1$ or $\ngtv{1}$) separately
to its "size"; for example, whether a number is pointing "up" or "down" the
number line.

### Normal (Canonical) Forms ###

Having multiple ways to write the same thing can be convenient during a
calculation, but we'd rather have a *single* way to write our "final result"; so
it can be easily compared to other results (by just comparing the symbols).

Such notation is called a "normal form" (or "canonical form"), and may be
familiar from dealing with fractions, where e.g. $\frac{27}{54}$ and
$\frac{3}{6}$ *appear* to be different numbers. We can write these in normal
form by removing common factors, to get $\frac{1}{2}$ and $\frac{1}{2}$; which
are obviously identical.

In the case of products, we can introduce and eliminate factors of $1$, and we
can expand, contract and shift around negatives between factors. Hence there are
many ways to write down the same value, and it would be nice to have a normal
form for any "final results". The most natural approach is the following:

 - Eliminate any factors of $1$. This makes the expression as small as possible.
 - Expand all negatives to cover the whole expression, since this avoids having
   to choose between factors arbitrarily.
 - Eliminate any "double negatives", to get only zero or one negative. This is
   easy once all the negatives have been expanded.

For example, the following expression:

$$3\ngtv{x}(\ngtv{y} + z) + \ngtv{x\ngtv{y}}$$

Would have the following normal form:

$$\ngtv{3x(\ngtv{y} + z)} + xy$$

Note that the $\ngtv{y} + z$ isn't itself a product, so its negative can't
expand or shift around. We could also "multiply out" and factorise, but that's
more about "sums and products" rather than negatives, so I'll leave it up to
taste, e.g.

$$x(4y + \ngtv{3z})$$
