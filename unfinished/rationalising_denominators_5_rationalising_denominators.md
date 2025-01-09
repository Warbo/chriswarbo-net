---
title: "Rationalising Denominators 5: Rationalising Denominators"
---

Posts in this series:

[Powers](/blog/2024-11-03-rationalising_denominators_1_powers.html)

[Products](/blog/2024-11-05-rationalising_denominators_2_products.html)

[Sums](/blog/2024-11-08-rationalising_denominators_3_sums.html)

[Ratios](/blog/2024-11-13-rationalising_denominators_4_ratios.html)

[Rationalising Denominators](/unfinished/rationalising_denominators_5_rationalising_denominators.html)

---

## Introduction ##

In [the previous
installment](/blog/2024-11-13-rationalising_denominators_4_ratios.html) we
introduced `Ratio` values, but they had a problem: the same numerical quantity
could be represented by multiple different `Ratio` values, like the following:

```python
# √2 - 1
Ratio(add_sums(sum_root_two, sum_neg), sum_one)

# 1/(1 + √2)
Ratio(sum_one, add_sums(sum_one, sum_root_two))
```

where `sum_one` represents 1, `sum_neg` represents -1 and `sum_root_two`
represents √2 (which is `Sum(Product(Power(2, Fraction(1, 2))))`{.python}). To
see why these two values represent the same number, try dividing the first
`Ratio` by itself (which equals one) and multiplying that by the second `Ratio`:

```
  (√2 - 1)/(√2 - 1) × 1/(1 + √2)
= ((√2 - 1) × 1)/((√2 - 1) × (1 + √2))
= (√2 - 1)/(√2 + 2 - 1 - √2)
= (√2 - 1)/1
= √2 - 1
```

The result is the first `Ratio`. Since we started by multiplying the second
`Ratio` by one, and multiplication by one doesn't change anything, this must be
equal to the second `Ratio`. Hence there are multiple ways to represent the same
numeric value.

In this post, we'll solve this problem by extending the normalisation algorithm
of our `Ratio` function, using a method called *rationalising the denominator*.
This ensures two equal numbers will always get the same `Ratio` representation.
However, to get there will require a bit of a journey!

### Zero is annoying ###

I'm mostly going to *ignore* zero in this post, since it would complicate the
explanations. We can't represent zero as a product of primes, so in practice we
use `Product(Power(0, one))`{.python}, but this introduces redundancies which
have to be normalised-away using extra rules, etc.

This is easy to handle via special-cases, so the code will work perfectly well
for zero; but I don't want to obscure the justifications with too many tangents.

## Radicals don't factorise uniquely ##

To normalise a `Ratio` we can try dividing the numerator and denominator by any
factors they have in common. For [natural numbers]() this is easy: each
factorises into a unique, finite set of primes; and those which don't share a
common prime factor do not share *any* common factor.

We lose this uniqueness if we allow factors to be arbitrary rationals, e.g. a
prime like 2 can be factorised further into rationals like 6 × ⅓, or many other
combinations. Thankfully we don't need to do that, since it doesn't allow us to
represent any more numbers than our top-level `Ratio` already can. However, once
we introduce radicals then fractional factors become unavoidable.

For example, consider:

```python
Ratio(
  Sum(Product(Power(2, Fraction(1, 2)))),
  Sum(Product(Power(2, Fraction(5, 6))))
)
```

This represents √2/(⁶√2⁵). Its numerator and denominator do not have any common
prime factors, but they *do* have radical factors in common, like
`Power(2, Fraction(1, 6))`, AKA ⁶√2; since √2 = ⁶√2 × ∛2 and ⁶√2⁵ = ⁶√2 × ∛2².
Dividing numerator and denominator by this common factor gives us:

```python
Ratio(
  Sum(Product(Power(2, Fraction(1, 3)))),
  Sum(Product(Power(2, Fraction(2, 3))))
)
```

These have a common factor of `Power(2, Fraction(1, 3))` (i.e. ∛2), which we can
divide through to get:

```python
Ratio(
  sum_one,
  Sum(Product(Power(2, Fraction(1, 3))))
)
```

Alternatively, we could have divided-through by 1/∛2 to get:

```python
Ratio(
  Sum(Product(Power(2, Fraction(2, 3)))),
  Sum(Product(Power(2, one)))
)
```

We could keep going, dividing by more and more radical factors, without ever
reaching a "base case" that no longer factors. We need a more systematic
approach, to avoid endless rearranging.

## Algebraic numbers ##

Radical expressions are [algebraic numbers](), which are defined as the
"zero-values" of [univariate polynomials]() (expressions involving sums and
products; containing *one* indeterminate, usually written as "x"). The
zero-values of such polynomials are those values of the [indeterminate]() which
make the whole polynomial expression zero (these are also called "roots", but
I'll avoid that terminology since it may be confused with [nth-roots]()).

As a trivial example, consider a natural number like 7: we'll think up an
expression which (a) includes 7, (b) uses only sums and products (so it could be
the result of evaluating a polynomial), and (c) equals zero. Here's one:

```
7 - 7 = 0
```

If we replace one of those 7 values with an indeterminate `x`, then we get a
polynomial equation like:

```
x - 7 = 0
```

This equation holds when `x` is a zero-value of this polynomial, and in this
case it holds when `x = 7`, so 7 is a zero-value of the polynomial `x - 7`.

Note that we could have swapped the second 7 instead, to get `7 - x = 0`, though
that's actually equivalent (just multiply both sides of the equation by -1). We
could also swap *both* occurrences of 7 with `x`, to get the equation
`x - x = 0`, which simplifies to `0 = 0`; but whilst that's *true*, it's not
*useful* for us, since it wouldn't constrain the value of `x` in any way!

Whilst these equations are *valid* for natural numbers, we can't use them as
*definitions*. For example, defining 7 as the zero-value of `x - 7` would be
circular, since that polynomial includes a 7 before we've defined it! Thankfully
we don't need to define the natural numbers this way (e.g. we could use
[Peano's axioms]() instead).

### Radicals as algebraic numbers ###

We can extend the above example by raising the indeterminate `x` to higher
powers, to get polynomials like `x² - 7`, `x³ - 7`, `x⁴ - 7`, and so on. The
zero-values of those are √7, ∛7, ⁴√7, and so on (check them for yourself!).

Since polynomials only involve sums and products, they cannot include radicals;
hence we *can* define radicals to be the zero-points of these associated
polynomials, with no possibility of circular reasoning. This gives us a concrete
semantics for our `Power` type, which also extends to `Product` and `Sum`
(albeit with more complicated polynomials).

The zero-points of *arbitrary* polynomials are the [algebraic numbers](); but
those are not necessarily pure radicals (like a `Power`) or a `Sum`/`Product`
involving pure radicals. Some may require more elaborate expressions like
[nested radicals]() which, since they do not have a canonical normal form, we
do not support in our representation.

### Algebraic conjugates ###

We now have a semantic interpretation for what our `Power`, `Product` and `Sum`
values *mean*, mathematically. However, there's a problem: polynomials
may have *many* zero-values, so the *particular* value of our expressions is not
defined *uniquely*! In fact, this shouldn't be surprising to anyone who learned
square roots in high school, since those can have *two* solutions: one positive
and the other negative. Indeed, we'll see that this idea of "negatives" can be
extended much further!

#### Multiplication dilates and rotates ####

in the
examples above we saw that √7 is a zero-value of `x² - 7`; but so is `-√7`. In
fact, linear polynomials (like `x - 7`) are the only ones that have a unique
zero-value (at least, up to [multiplicity]()), so we can't "the" zero-value and that remembers  that radical expressions This may be
familiar definition *isn't unique*The has this is the solution of an
re going to take a detour The defining feature of a radical is that it solves a polynomial equation.

## Roots Of Unity ##

Symbolically ζₙ represents `Power(1, Fraction(1, n))`. Field extension:
adjoining a new symbol, quotienting-out polynomial x^n - 1 = 0.

### As A Group ###
×
Since it's closed, contains identity (one) for multiplication. Cyclic group,
since repeated multiplication (i.e. incrementing the power) gets back to
starting point (since we eventually get to one, then a further multiplication
results in the starting point).

Inverses exist, since they're whatever's remaining in order to reach the number
one. For example, ζ₇⁵ is less than one

### As A Picture ###

## Conjugates ##

##
