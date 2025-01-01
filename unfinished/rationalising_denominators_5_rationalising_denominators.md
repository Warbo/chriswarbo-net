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

We finished [the previous installment]() with a problem: distinct `Ratio`
representations which should normalise to the same value, like the following:

```python
sum_root_two = Sum(Product(Power(2, Fraction(1, 2))))

# √2 - 1
Ratio(add_sums(sum_root_two, sum_neg), sum_one)

# 1/(1 + √2)
Ratio(sum_one, add_sums(sum_one, sum_root_two))
```

In this post, we'll extend the normalisation algorithm in the `Ratio` function
to *rationalise the denominator*, and hence give every `Ratio` value a single,
normal representation. However, to get there will require a bit of a journey!

## Algebraic conjugates ##

To multiply a fraction, we just multiply its numerator (then normalise the
result). To divide a fraction, we multiply its denominator (assuming our
multiplier is non-zero). Multiplying both the numerator *and* denominator leaves
the value unchanged, but it may cause changes to our `Sum` representation. In
particular, we can always find a
can use this to normalise a `Ratio`, by we're going to look

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
