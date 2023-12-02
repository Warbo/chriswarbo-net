---
title: "A New Numeric Tower: Sums And Products"
---

## Introduction ##

We tend to learn about sums ("adding numbers") quite early, either when starting
school or even before. We also learn about products ("times-ing numbers")
shortly after. We often represent each using a table, to show their
patterns. For example:

<figure>

```
┏━━━┳━━━┯━━━┯━━━┯━━━┯━━━┓
┃ + ┃ 2̅ │ 1̅ │ 0 │ 1 │ 2 ┃
┣━━━╋━━━┿━━━┿━━━┿━━━┿━━━┫
┃ 2̅ ┃ 4̅ │ 3̅ │ 2̅ │ 1̅ │ 0 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 1̅ ┃ 3̅ │ 2̅ │ 1̅ │ 0 │ 1 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 0 ┃ 2̅ │ 1̅ │ 0 │ 1 │ 2 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 1 ┃ 1̅ │ 0 │ 1 │ 2 │ 3 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 2 ┃ 0 │ 1 │ 2 │ 3 │ 4 ┃
┗━━━┻━━━┷━━━┷━━━┷━━━┷━━━┛
```

 <figcaption>Addition table for integers</figcaption>
</figure>

<figure>

```
┏━━━┳━━━┯━━━┯━━━┯━━━┯━━━┓
┃ × ┃ 2̅ │ 1̅ │ 0 │ 1 │ 2 ┃
┣━━━╋━━━┿━━━┿━━━┿━━━┿━━━┫
┃ 2̅ ┃ 4 │ 2 │ 0 │ 2̅ │ 4̅ ┃
┠───╂───┼───┼───┼───┼───┨
┃ 1̅ ┃ 2 │ 1 │ 0 │ 1̅ │ 2̅ ┃
┠───╂───┼───┼───┼───┼───┨
┃ 0 ┃ 0 │ 0 │ 0 │ 0 │ 0 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 1 ┃ 2̅ │ 1̅ │ 0 │ 1 │ 2 ┃
┠───╂───┼───┼───┼───┼───┨
┃ 2 ┃ 4̅ │ 2̅ │ 0 │ 2 │ 4 ┃
┗━━━┻━━━┷━━━┷━━━┷━━━┷━━━┛
```

 <figcaption>Multiplication table for integers</figcaption>
</figure>

However, the ideas of sums and products have been generalised much further than
just numbers. So what about these generalisations makes them "sums" and
"products"; what makes other things *not* sums or products; and what's the
*difference* between a "sum" and a "product"?

This post will explore these ideas; and, for the adventurous, we'll implement
these ideas in code, using the Racket language. Racket is written in [prefix
form]() (AKA [s-expressions]()), which may be unfamiliar but is hopefully
straightforward: expressions are written `(in parentheses)`, with the operation
first and its inputs afterwards. For example `(+ 1 2 3)`{.scheme} is the sum of
`1`{.scheme}, `2`{.scheme} and `3`{.scheme}; whilst `(× 7 (+ 4 x))`{.scheme} is
the product of `7`{.scheme} and `(+ 4 x)`{.scheme}, with the latter being the
sum of `4`{.scheme} and some variable `x`{.scheme}. We can represent equations
like this:

```scheme
(= (+ 1 2 (× 3 (+ 4 5)))
   (+ 3   (× 3 (+ 4 5)))
   (+ 3   (+ (× 3 4) (× 3 5)))
   (+ 3      (× 3 4) (× 3 5))
   (+ 3      12      (× 3 5))
   (+ 15             (× 3 5))
   (+ 15             15)
   (× 15 2)
   30)
```

## Types ##

We can think of addition and multiplication as *operations* which transform
input values into an output value. A fundamental aspect of these values is their
*type*: what sort of things are we talking about? For the addition and
multiplication we learn in school, all of the values are *numbers*: they have
type number. They could be literal numbers, like `12`{.scheme}; or variables
representing unknown numbers, like `x`{.scheme}; or the result of some numerical
operation, like `(+ 1 2 3)`{.scheme}. This means the *output* of one sum/product
can be used as *input* to another, so they can be nested arbitrarily.

When we extend the ideas of sum and product to more general situations, we
always require the the input and output types to match, so they can be nested.
For example, the product of intervals is an interval; the sum of random
variables is a random variable; etc. Any operation that combines things of
*different* types, or whose output has a different type to its inputs, is
usually not considered to be a sum or a product.

## Laws of Algebra ##

Algebra focuses on *operations*, and the *laws* they obey. Some laws (known as
[axios]()) are obeyed by definition, e.g we define a "group" as anything which
obeys the axioms of group theory, so therefore every group obeys those laws.
Other laws (known as [theorems]()) are an inevitable consequence of certain
axioms. We'll look at a few laws which are important for sums and products
(don't worry about their horrible names!)

Note: Programmers may refer to laws as [invariants]() or [properties](). They
are usually well-suited to automated testing, e.g. via [property checking]().

### The Law Of Associativity ###

This law holds for any operation
