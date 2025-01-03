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
`exponent`{.python}](https://en.wikipedia.org/wiki/Exponentiation):

The `base`{.python} can be any
[`int`{.python}eger](https://en.wikipedia.org/wiki/Integer), i.e. a positive
whole number, or a negative whole number, or zero.

The `exponent`{.python} can be a
[`Fraction`{.python}](https://en.wikipedia.org/wiki/Fraction) (a
`numerator`{.python} over a `denominator`{.python}), but must obey certain
rules:

 - We do not allow negative `exponent`{.python}s
 - If the `base`{.python} is zero, the `exponent`{.python} cannot also be zero.
 - If the `base`{.python} is negative, the `exponent`{.python} must be a whole
   number; i.e. its `denominator`{.python} must be one.

Here's a simple implementation in Python:

```python
from fractions import Fraction

def Power(base: int, exp: Fraction) -> Tuple[int, Fraction]:
    assert exp >= 0, f"Power {base}^{exp} cannot have negative exponent"
    if base == 0:
        assert exp != 0, f"Power 0^0 is undefined"
    if base < 0:
        assert exp.denominator == 1, \
            f"Negative power {base}^{exp} has fractional exponent"
    return (base, exp)
```

I've given this function an uppercased name, to indicate that we'll use
`Power`{.python} as a type annotation as well as for constructing values. Here
are some useful constants of this type:

```python
power_zero = Power(0, Fraction(1, 1))
power_one = Power(1, Fraction(1, 1))
power_neg = Power(-1, Fraction(1, 1))
```

## Normalisation ##

Thankfully, Python's `Fraction`{.python} will automatically reduce values to
their "normal form", e.g. calling `Fraction(2, 4)`{.python} will return the
value `Fraction(1, 2)`{.python}. However, there are other redundancies in our
`Power`{.python} type that will not simplify automatically; especially values
involving the numbers zero, one and negative one. For example the following
values all represent the number one:

 - `Power(1, Fraction(1, 1))`{.python}
 - `Power(1, Fraction(2, 1))`{.python}
 - `Power(2, Fraction(0, 1))`{.python}
 - `Power(-1, Fraction(2, 1))`{.python}

### Powers Of Zero ###

When the `base`{.python} is zero, we don't allow the exponent to be zero (since
that's not well-defined mathematically). For every *other* exponent, there is
redundancy, since zero raised to any non-zero power is zero. We can avoid this
redundancy by choosing a particular exponent to be "normal", and replace all
other `exponent`{.python}s of zero with the normal `exponent`{.python}. I'll
pick the number one to be our normal `exponent`{.python}, and add the following
lines to our `Power`{.python} function to perform this normalisation:

```python
    if base == 0:
        exp = Fraction(1, 1)
```

### Powers Of One ###

When the `base`{.python} is one, we can add one to the `exponent`{.python}
without changing the overall value; since that corresponds to multiplying the
result by the `base`{.python}, which in this case means multiplying by one,
which is redundant. We can reduce these exponents to avoid this redundancy, by
repeatedly taking away one until it becomes less than one. This corresponds to
the [modulo](https://en.wikipedia.org/wiki/Modulo) operation, with
[modulus](https://en.wikipedia.org/wiki/Modular_arithmetic#Congruence) of one:

```python
    if base == 1:
        exp = exp % 1
```

### Powers Of Negative One ###

When the `base`{.python} is *negative one*, we can add *two* to the
`exponent`{.python} without changing the overall value (adding one will negate
the value; [negating twice is
redundant](https://en.wikipedia.org/wiki/Involution_(mathematics))). Hence we
can reduce exponents using a modulus of *two*:

```python
    if base == -1:
        exp = exp % 2
```

### Zeroth Powers ###

Our final normalisation applies when the `exponent`{.python} is zero: any
non-zero number raised to the power of zero gives a result of one. Hence we can
choose a "normal" value for the `base`{.python}, say the number one, and replace
any other `base`{.python} with that:

```python
    if exp == 0:
        base = 1
```

This complements the previous two rules, since those modulo operations could
result in the values `Power(1, Fraction(0, 1))`{.python} or
`Power(-1, Fraction(0, 1))`{.python}. This rule chooses the former as the
"normal form", and replaces the latter.

## Conclusion ##

Here's our overall implementation of `Power`{.python}:

```python
from fractions import Fraction

def Power(base: int, exp: Fraction) -> Tuple[int, Fraction]:
    assert exp >= 0, f"Power {base}^{exp} cannot have negative exponent"
    if base == 0:
        assert exp != 0, f"Power 0^0 is undefined"
        exp = Fraction(1, 1)
    if base < 0:
        assert exp.denominator == 1, \
            f"Negative base {base} has fractional exponent {exp}"
    if base == 1:
        exp = exp % 1
    if base == -1:
        exp = exp % 2
    if exp == 0:
        base = 1
    return (base, exp)

power_zero = Power(0, Fraction(1, 1))
power_one = Power(1, Fraction(1, 1))
power_neg = Power(-1, Fraction(1, 1))
```

So far this is a pretty simple way to represent numbers, but it turns out to be
pretty powerful. We've implemented some normalisation steps, but there are still
some redundancies; e.g. the number four can be represented in many ways, like:

 - `Power(4, Fraction(1, 1))`{.python}
 - `Power(2, Fraction(2, 1))`{.python}
 - `Power(-2, Fraction(2, 1))`{.python}
 - `Power(16, Fraction(1, 2))`{.python}
 - etc.

In [the next post](/blog/2024-11-05-rationalising_denominators_2_products.html)
we'll extend this to *products* of powers.
