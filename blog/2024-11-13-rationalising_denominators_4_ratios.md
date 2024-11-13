---
title: "Rationalising Denominators 4: Ratios"
---

Posts in this series:

[Powers](/blog/2024-11-03-rationalising_denominators_1_powers.html)

[Products](/blog/2024-11-05-rationalising_denominators_2_products.html)

[Sums](/blog/2024-11-08-rationalising_denominators_3_sums.html)

[Ratios](/blog/2024-11-13-rationalising_denominators_4_ratios.html)

---

## Introduction ##

So far we've represented numbers using:

 - `Power`, which raises an `int`eger value to a `Fraction`al `exponent`
 - `Product`, which contains a list of `Power` values
 - `Sum`, which contains a list of `Product` values

In this post, we introduce a `Ratio` of two `Sum` values, which is the most
expansive numerical representation in this series (if you want to go even
further, you might like my [Ivory Tower library](/projects/ivory), which is very
much a work-in-progress at the moment!)

## Ratios ##

Our initial implementation is simply a tuple of two values, representing the
numerator and denominator. We'll forbid zero as a denominator, since that isn't
well-defined:

```python
def Ratio(numerator: Sum, denominator: Sum) -> Tuple[Sum, Sum]:
    assert denominator != sum_zero

    # 0/n normalises to 0/1
    if numerator == sum_zero:
        return (sum_zero, sum_one)

    return (numerator, denominator)
```

I've also added a simple normalisation step for `Ratio` values whose numerator
is zero: since every such `Ratio` is equivalent (they are all the number zero),
we make their representations the same by replacing their denominators with the
number one.

## Arithmetic With Ratios ##

Here are some useful constants:

```python
ratio_zero = Ratio(sum_zero, sum_one)
ratio_one = Ratio(sum_one, sum_one)
ratio_neg = Ratio(sum_neg, sum_one)
```

Addition and multiplication are defined as usual for fractions:

```python
def add_ratios(r1: Ratio, r2: Ratio) -> Ratio:
    n1, d1 = r1
    n2, d2 = r2
    return Ratio(
        add_sums(multiply_sums(n1, d2), multiply_sums(n2, d1)),
        multiply_sums(d1, d2)
    )

def multiply_ratios(r1: Ratio, r2: Ratio) -> Ratio:
    n1, d1 = r1
    n2, d2 = r2
    return Ratio(multiply_sums(n1, n2), multiply_sums(d1, d2))
```

## Normalising To Lowest Terms ##

In this post we'll only cover one part of the normalisation process: reducing to
lowest terms, by removing common factors. Since our `Product` representation is
already broken down into prime factors, we can perform this normalisation by
removing those factors which occur in every term of the numerator and
denominator.

### Finding Common Factors ###

To find the common factors of a `Sum` we begin with the contents of its first
`Product`, then look through any remaining `Product`s to keep only those
`Power`s whose `base` appears in all of them. We take the minimal `exponent` for
each factor, since that can be subtracted without turning any `exponent`
negative (which we don't allow):

```python
def common_factors_of_sum(s: Sum) -> Product:
    """Returns a Product whose Powers appear in every term of the given Sum.
    This takes into account bases with differing exponents, by returning the
    smallest."""
    return product_one if s == sum_zero else Product(*reduce(
        lambda common, product: {
            base: min(common[base], product[base])
            for base in common
            if base in product
        },
        map(dict, s[1:]),
        dict(s[0])
    ).items())
```

### Removing Common Factors ###

We can use the following `remove_common_product` function to "divide through"
a `Sum` by a `Product` of its common factors, which we implement by subtracting
the common `exponent` of each `base` in each term:

```python
def remove_common_product(s: Sum, p: Product) -> Sum:
    return Sum(*reduce(
        remove_common_factor,
        p,
        s
    ))

def remove_common_factor(s: Sum, p: Power) -> Sum:
    base, exp = p
    return Sum(*[
        Product(*[Power(b, e - exp if b == base else e) for b, e in product])
        for product in s
    ])
```

### Reducing Ratios ###

To reduce `Ratio` values to lowest terms, we just need to extend our `Ratio`
function to find and remove common factors:

```python
def Ratio(numerator: Sum, denominator: Sum) -> Tuple[Sum, Sum]:
    assert denominator != sum_zero

    # 0/n normalises to 0/1
    if numerator == sum_zero:
        return (sum_zero, sum_one)

    # Find common factors in the numerator and denominator
    num_factors = dict(common_factors_of_sum(numerator))
    common_product = Product(*{
        base: min(exp, num_factors[base])
        for base, exp in common_factors_of_sum(denominator)
        if base in num_factors
    }.items())

    # Remove common factors from top and bottom of ratio
    return (
        remove_common_product(numerator, common_product),
        remove_common_product(denominator, common_product)
    )
```

## No Negative Denominators ##

There is another redundancy in `Ratio`, since values like
`Ratio(sum_neg, sum_one)` and `Ratio(sum_one, sum_neg)` are equivalent (they are
`-1/1` and `1/(-1)`, respectively). We can transform between these by
multiplying both numerator and denominator by negative one; which we'll use to
normalise whenever the denominator has a common factor of negative one:

```python
    # Remove common factors from top and bottom of ratio
    num_reduced, den_reduced = (
        remove_common_product(numerator, common_product),
        remove_common_product(denominator, common_product)
    )

    # Prefer negatives to be in the numerator
    multiplier = \
        sum_neg if -1 in dict(common_factors_of_sum(den_reduced)) else sum_one
    return (
        multiply_sums(multiplier, num_reduced),
        multiply_sums(multiplier, den_reduced)
    )
```

## Fixed Point ##

It's not strictly needed here, but for consistency with the normalisation
procedures of our other types we'll have our normalisation keep repeating until
the `Ratio` reaches a fixed point:

```python
    result = (
        multiply_sums(multiplier, reduced_num),
        multiply_sums(multiplier, reduced_den)
    )
    # Recurse if we haven't reached a fixed point
    return result if result == (numerator, denominator) else Ratio(*result)
```

## Conclusion ##

In this post we've introduced a `Ratio` type. With this, we can finally
represent fractions, like a half:

```python
Ratio(
    Sum(Product()),
    Sum(Product(Power(2, Fraction(1, 1))))
)
```

This may seem rather convoluted, considering that we *started* with `Fraction`
right at the beginning (it's there in the above example!). Alternatively, we
could have supported fractions via negative `exponent`s, but instead we
explicitly forbade them. Indeed, negative `exponent`s would behave perfectly
reasonably, and work nicely with our normalisation algorithms.

However, a problem arises regarding *fractional powers* (i.e. roots), since they
break our assumptions regarding divisibility and factoring. For example,
consider the value `1/(1 + √2)`, which we could write like:

```python
sum_root_two = Sum(Product(Power(2, Fraction(1, 2))))
numerator = sum_one
denominator = add_sums(sum_one, sum_root_two)
Ratio(numerator, denominator)
```

If we multiply the `numerator` and `denominator` by the same non-zero value, we
should get back the same result; since that's equivalent to multiplying by one.
However, if we try that with `1 - √2` it won't work:

```python
sum_one_minus_root_two = add_sums(sum_one, multiply_sums(sum_neg, sum_root_two))

new_numerator = multiply_sums(numerator, sum_one_minus_root_two)
new_denominator = multiply_sums(denominator, sum_one_minus_root_two)
old = Ratio(numerator, denominator)
new = Ratio(new_numerator, new_denominator)
assert new == old, f"{new} should equal {old}"
```

```python
AssertionError: ([[(-1, Fraction(1, 1))], [(2, Fraction(1, 2))]], [[]]) should
equal ([[]], [[], [(2, Fraction(1, 2))]])
```

Decoding these values, we see that the normal form for `old` is unchanged from
how we wrote it, as `1/(1 + √2)`; whilst `new` has normalised to
`(-1 + √2)/1`, or `√2 - 1`. It turns out that these *are* equivalent values, but
reducing them down to the same representation requires a more sophisticated
normalisation algorithm, called "rationalising the denominator", which this
whole series has been building to.

Next time we'll see how to rationalise denominators; but in order to implement
that, we need a way to split values into a numerator and a denominator. Keeping
them separate using this `Ratio` type makes that trivial; whereas allowing
fractions or negative exponents inside sums and products would be much more
tedious to unravel!

## Appendices ##

### Implementation ###

```python

def Ratio(numerator: Sum, denominator: Sum) -> Tuple[Sum, Sum]:
    assert denominator != sum_zero

    # 0/n normalises to 0/1
    if numerator == sum_zero:
        return (sum_zero, sum_one)

    # Find common factors in the numerator and denominator
    num_factors = dict(common_factors_of_sum(numerator))
    common_product = Product(*{
        base: min(exp, num_factors[base])
        for base, exp in common_factors_of_sum(denominator)
        if base in num_factors
    }.items())

    # Remove common factors from top and bottom of ratio
    num_reduced, den_reduced = (
        remove_common_product(numerator, common_product),
        remove_common_product(denominator, common_product)
    )

    # Prefer negatives to be in the numerator
    multiplier = \
        sum_neg if -1 in dict(common_factors_of_sum(den_reduced)) else sum_one
    result = (
        multiply_sums(multiplier, num_reduced),
        multiply_sums(multiplier, den_reduced)
    )

    # Recurse if we haven't reached a fixed point
    return result if result == (numerator, denominator) else Ratio(*result)

def add_ratios(r1: Ratio, r2: Ratio) -> Ratio:
    n1, d1 = r1
    n2, d2 = r2
    return Ratio(
        add_sums(multiply_sums(n1, d2), multiply_sums(n2, d1)),
        multiply_sums(d1, d2)
    )

def multiply_ratios(r1: Ratio, r2: Ratio) -> Ratio:
    n1, d1 = r1
    n2, d2 = r2
    return Ratio(multiply_sums(n1, n2), multiply_sums(d1, d2))

def common_factors_of_sum(s: Sum) -> Product:
    """Returns a Product whose Powers appear in every term of the given Sum.
    This takes into account bases with differing exponents, by returning the
    smallest."""
    return product_one if s == sum_zero else Product(*reduce(
        lambda common, product: {
            base: min(common[base], product[base])
            for base in common
            if base in product
        },
        map(dict, s[1:]),
        dict(s[0])
    ).items())

def remove_common_product(s: Sum, p: Product) -> Sum:
    return Sum(*reduce(
        remove_common_factor,
        p,
        s
    ))

def remove_common_factor(s: Sum, p: Power) -> Sum:
    base, exp = p
    return Sum(*[
        Product(*[Power(b, e - exp if b == base else e) for b, e in product])
        for product in s
    ])

ratio_zero = Ratio(sum_zero, sum_one)
ratio_one = Ratio(sum_one, sum_one)
ratio_neg = Ratio(sum_neg, sum_one)
```
