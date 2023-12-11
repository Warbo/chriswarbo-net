---
title: "Ivory: Sums And Products"
packages: ['racketWithRackCheck']
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
these ideas in Racket code.

## Types ##

We can think of addition and multiplication as *operations* or *functions* which
transform "input values" into an "output value". A fundamental aspect of these
values is their *type*: what sort of things are we talking about?

For the addition and multiplication we learn in school, all of the values are
*numbers*; i.e. they have type number. They could be "literal" numbers, like
`12`{.scheme}; or "variables" which *represent* some number, like `x`{.scheme};
or the output of some other numerical operation, like `(+ 1 2)`{.scheme}.

Having the same type for inputs and outputs allows sums and products to be
*nested* in arbitrary ways: with the output one one used as the input of
another, forming "trees":

<figure>

```
    +
  ┌─┴─┐
  │   │
  ×   3
┌─┴─┐
│   │
7   ×
  ┌─┴─┐
  │   │
  2   9
```

 <figcaption>A tree of nested sums and products, equivalent to the s-expression
 `(+ (× 7 (× 2 9)) 3)`{.scheme}, or the infix expression $(7 × (2 × 9)) + 3$.
</figure>

Any operation that combines things of *different* types, or whose output has a
different type to its inputs, is not usually considered a sum or a product.

When we extend the ideas of sum and product to more general situations, we
always require the the input and output types to match, so they can be nested.
For example, the product of intervals is an interval; the sum of random
variables is a random variable; etc.

## Laws of Algebra ##

We can characterise operations *algebraically* by stating "laws": equations
which an operation always satisfies, regardless of its inputs (written ). by
that which sum and products using equations, which tell us when different forms
of nesting are equal. These are *algebraic laws*, although programmers may refer
to them as
[invariants](https://en.wikipedia.org/wiki/Invariant_(mathematics)#Invariants_in_computer_science)
or
[properties](https://hypothesis.works/articles/what-is-property-based-testing/).

### The Law Of Associativity ###

<figure>

```
   +                   +                      +
   │                   │                      │
┌──┴──┐             ┌──┴──┐             ┌─────┼─────┐
│     │             │     │             │     │     │
a     +      =      +     c      =      a     b     c
      │             │
   ┌──┴──┐       ┌──┴──┐
   │     │       │     │
   b     c       a     b
```

---

```
   ×                   ×                      ×
   │                   │                      │
┌──┴──┐             ┌──┴──┐             ┌─────┼─────┐
│     │             │     │             │     │     │
a     ×      =      ×     c      =      a     b     c
      │             │
   ┌──┴──┐       ┌──┴──┐
   │     │       │     │
   b     c       a     b
```

<figcaption>The law of associativity for `+` and `+`. These equations always
hold, regardless of the values of `a`, `b` and `c`.
</figure>

This law states how nesting an operation inside that same operation works, like
having a sum-of-sums or a product-of-products. It tells us the following are
always equal, regardless of what `x`, `y` and `z` are:

```scheme
(= (+ x (+ y z))
   (+ (+ x y) z)
   (+ x y z))
   
(= (× x (× y z))
   (× (× x y) z)
   (× x y z))
```

It may be easier to see what's happening by drawing these as trees:



There are a few things to notice about these equations. Firstly, the inputs `x`,
`y` and `z` always appear in the same order: associativity does not let us swap
around values. Instead, it lets us "rebalance" nested operations; as a tree

says the following should worknesting a su an operation inside *the same* operation wor

### Representing Sums And Products in Scheme ###

Since products and sums don't always "reduce" to something simpler, we'll use
the same "uninterpreted function" trick as we did for representing GA units.
This time, since we're dealing with products and sums, we'll use the symbols `×`
and `+`. Also, since they can accept any amount of inputs, we'll keep them in a
list rather than a pair.

This makes the implementation of multiplication and addition pretty simple: we
just wrap all of the inputs into a product or sum, respectively. We then apply a
`canonical` function (defined in the next section), to rearrange them into our
preferred form (sum-of-products, in alphabetical order):

```{.scheme pipe="./show"}
;; Shorthands for creating products and sums
(define (geo-× . args) (canonical (cons '× args)))
(define (geo-+ . args) (canonical (cons '+ args)))

;; Predicates for spotting products, sums and general geometric numbers

(define/match (geo-×? n)
  [((cons '× ns)) (andmap geometric? ns)]
  [(_) #f])

(define/match (geo-+? n)
  [((cons '+ ns)) (andmap geometric? ns)]
  [(_) #f])

(define geo? (disjoin unit-ga? geo-×? geo-+?))

(define geometric? (disjoin number? geo?))
```
