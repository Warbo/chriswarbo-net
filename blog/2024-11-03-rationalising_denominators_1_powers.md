---
title: "Rationalising Denominators 1: Fractional Powers"
---

Posts in this series:

[Powers](/blog/2024-11-03-rationalising_denominators_1_powers.html)

[Products](/blog/2024-11-05-rationalising_denominators_2_products.html)

[Sums](/blog/2024-11-08-rationalising_denominators_3_sums.html)

[Ratios](/blog/2024-11-13-rationalising_denominators_4_ratios.html)

---

## Introduction ##

I've spent the last few weeks playing around with
[radicals](https://en.wikipedia.org/wiki/Solution_in_radicals), looking for a
simple representation that will fit neatly into my
[Ivory Tower](/projects/ivory) library. After a few false starts,
I've cobbled together a neat little Python library, which I thought was worth
sharing across a few blog posts.

## Powers ##

We'll start by defining a `Power`{.python} as a pair of numbers, which we'll
call [a `base`{.python} and an
`exp`{.python}onent](https://en.wikipedia.org/wiki/Exponentiation):

The `base`{.python} can be any [natural number](), i.e. a whole number which is
either positive or zero.

The `exp`{.python}onent can be a
[`Fraction`{.python}](https://en.wikipedia.org/wiki/Fraction) (a
`numerator`{.python} over a `denominator`{.python}), but must obey certain
rules:

 - We do not allow negative `exp`{.python}onents
 - If the `base`{.python} is 0, the `exp`{.python}onent cannot also be 0.

Here's a simple implementation in Python:

```python
from fractions import Fraction

def Power(base: int, exp: Fraction) -> Tuple[int, Fraction]:
    assert base >= 0, f"Power {base}^{exp} cannot have negative base"
    assert exp >= 0, f"Power {base}^{exp} cannot have negative exponent"
    if base == 0:
        assert exp != 0, f"Power 0^0 is undefined"
    return (base, exp)
```

I've given this function an uppercased name, to indicate that we'll use
`Power`{.python} as a type annotation as well as for constructing values. Here
are some constants for this `Power`{.python} type, as well as for
`exp`{.python}onents:

```python
zero = Fraction(0, 1)
one = Fraction(1, 1)
half = Fraction(1, 2)

power_zero = Power(0, one)
power_one = Power(1, one)
```

## Normalisation ##

Thankfully, Python's `Fraction`{.python} will automatically reduce values to
their "normal form", e.g. calling `Fraction(2, 4)`{.python} will return the
value `Fraction(1, 2)`{.python}. However, there are other redundancies in our
`Power`{.python} type that will not simplify automatically; especially values
involving the numbers 0 and 1. For example the following values all represent
the number 1:

 - `Power(1, Fraction(1, 1))`{.python}
 - `Power(1, Fraction(2, 1))`{.python}
 - `Power(2, Fraction(0, 1))`{.python}

### Powers of zero ###

When the `base`{.python} is 0, we don't allow the `exp`{.python}onent to be 0
(since that's not well-defined mathematically). For every *other*
`exp`{.python}onent, there is redundancy, since 0 raised to any non-zero power
is 0. We can avoid this redundancy by choosing a particular `exp`{.python}onent
to be "normal", and replace all other `exp`{.python}onents of 0 with the normal
`exp`{.python}onent. I'll pick the number 1 to be our normal `exp`{.python}onent
and add the following lines to our `Power`{.python} function to perform this
normalisation:

```python
    if base == 0:
        exp = one
```

### Powers of one ###

When the `base`{.python} is 1, we can add 1 to the `exp`{.python}onent without
changing the overall value; since that corresponds to multiplying the result by
the `base`{.python}, which in this case means multiplying by 1, which is
redundant. We can reduce these `exp`{.python}onents to avoid this redundancy, by
repeatedly taking away 1 until it becomes less than 1. This corresponds to the
[modulo](https://en.wikipedia.org/wiki/Modulo) operation, with
[modulus](https://en.wikipedia.org/wiki/Modular_arithmetic#Congruence) of 1:

```python
    if base == 1:
        exp = exp % 1
```

### Zeroth powers ###

Our final normalisation applies when the `exp`{.python}onent is 0: any non-zero
number raised to the power of 0 gives a result of 1. Hence we can choose a
"normal" value for the `base`{.python}, say the number 1, and replace any other
`base`{.python} with that:

```python
    if exp == 0:
        base = 1
```

This complements the previous rule, since the modulo operation could result in
the value `Power(1, Fraction(0, 1))`{.python}.

## Conclusion ##

Here's our overall implementation of `Power`{.python}:

```python
from fractions import Fraction

def Power(base: int, exp: Fraction) -> Tuple[Base, Exponent]:
    assert base >= 0, f"Power {base}^{exp} cannot have negative base"
    assert exp >= 0, f"Power {base}^{exp} cannot have negative exponent"
    if base == 0:
        assert exp != 0, f"Power 0^0 is undefined"
        # Normalise all other powers of 0 to 0^1, since they're equivalent
        exp = one
    if base == 1:
        # Remove whole powers of 1, since they just multiply by one
        exp = exp % 1
    if exp == 0:
        # Anything to the power of zero is one. Normalise to 1^1.
        base, exp = (1, one)
    return (base, exp)

zero = Fraction(0, 1)
one = Fraction(1, 1)
half = Fraction(1, 2)

power_zero = Power(0, Fraction(1, 1))
power_one = Power(1, Fraction(1, 1))
```

So far this is a pretty simple way to represent numbers, but it turns out to be
quite powerful. We've implemented some normalisation steps, but there are still
some redundancies; e.g. the number 4 can be represented in many ways, like:

 - `Power(4, one)`{.python}
 - `Power(2, Fraction(2, 1))`{.python}
 - `Power(16, half)`{.python}
 - etc.

In [the next post](/blog/2024-11-05-rationalising_denominators_2_products.html)
we'll extend this to *products* of powers.
