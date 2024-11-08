---
title: "Rationalising Denominators 3: Sums"
---

Posts in this series:

[Powers](/blog/2024-11-03-rationalising_denominators_1_powers.html)

[Products](/blog/2024-11-05-rationalising_denominators_2_products.html)

[Sums](/unfinished/rationalising_denominators_3_sums.html)

---

## Introduction ##

In the [previous post]() we generalised our `Power` type into a `Product` of
arbitrarily-many powers multiplied together. However, there are values which
can't be represented that way, such as 1+√7. Instead, these can be represented
as a *sum* of such products, which we'll define in this post.

## Sums ##

We define `Sum` in a similar way to `Product`, as a list of Python values:

```python
def Sum(*products: Product) -> List[Product]:
    return list(products)
```

## Arithmetic With Sums ##

Here are some useful constants:

```python
sum_zero = Sum(product_zero)
sum_one = Sum(product_one)
sum_neg = Sum(product_neg)
```

Adding `Sum` values is easy, just concatenate their lists:

```python
def add_sums(*sums) -> Sum:
    return Sum(*sum(sums, []))
```

To multiply two `Sum` values, we multiply [each element of the first with
each element of the second](https://en.wikipedia.org/wiki/Cartesian_product),
and take the `Sum` of all those. We can generalise this to *any* number of `Sum`
values by starting with `sum_one` and multiplying it by each of the given values
one after another (the order doesn't matter, since we're assuming it's
[associative](https://en.wikipedia.org/wiki/Associative_property) and
[commutative](https://en.wikipedia.org/wiki/Commutative_property)). In
programming terms this is a
[fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)#On_lists)
(which Python insists on calling "`reduce`"…):

```python
def multiply_sums(*sums) -> Sum:
    return reduce(
        lambda s1, s2: Sum(*[
            multiply_products(p1, p2)
            for p1 in s1
            for p2 in s2
        ]),
        sums,
        sum_one
    )
```

Finally, this helper tells us when a `Sum` contains no roots (i.e. no fractional
powers):

```python
def sum_is_rational(s: Sum) -> bool:
    return all(product_is_rational(p) for p in s)
```

## Normalising ##

Just like our other representations, we want to normalise `Sum` values, so that
each number is represented in exactly one way.

### Permutations ###

The order of elements in a `Sum` does not affect its value, so we can sort them
into a canonical order (the precise order doesn't matter, so long as elements
are always sorted in the same way):

```python
def Sum(*products: Product) -> List[Product]:
    return sorted(list(products))
```

### Common Factors ###

When two elements of a `Sum` have a common set of fractional/irrational `Power`s
they can be combined into a single `Product`. We do this in two steps. First, we
split each `Product` into a rational part (containing no fractional `exponent`s)
and an irrational part (containing only fractional `exponent`s); if a `Product`
lacks such `Power`s, we use the number one instead:

```python
def split_rationals(product: Product) -> Tuple[Product, Product]:
    if product == product_zero:
        return (product_zero, product_one)
    go = lambda power: reduce(
        multiply_products,
        [Product(Power(base, power(exp))) for base, exp in product],
        product_one
    )
    return (
        go(lambda e: e // 1), # rationals
        go(lambda e: e %  1)  # irrationals
    )
```

We can combine those products whose irrational parts are the same, by adding
their rational parts:

```python
def irrationals_coefficients(products_list: Sum):
    result = {}
    for product in products_list:
        rationals, irrationals = split_rationals(product)
        key = frozenset(irrationals)
        if key not in result:
            result[key] = 0
        result[key] += eval_product_int(rationals)
    return result
```

This will also take care of "cancelling out" positives and negatives, like
`Sum(product_one, product_neg)`.

We use `eval_product_int` (defined in the previous post) to turn the rational
part of a `Product` into a raw Python `int`; this is possible since the rational
part won't contain any roots (by definition), and hence is just some common
multiple of the `base` `int`egers.

To incorporate this into our normalisation, we turn each of these `int` values
into a `Power` with `exponent` one, and wrap in a `Product`:

```python
def Sum(*products: Product) -> List[Product]:
    # This Sum has a term for each distinct irrational product, multiplied by
    # the sum of all the rationals it appeared with in the input
    coefficients = irrationals_coefficients(products)
    return sorted([
        multiply_products(
            Product(*irrational),
            Product(Power(coeff, Fraction(1, 1)))
        )
        for irrational, coeff in coefficients.items()
    ])
```

### Skipping Zeroes ###

Adding zero to a `Sum` does not change its value, so we filter out such elements
(similar to how we filter out ones from a `Product`):

```python
    return = sorted([
        multiply_products(
            sorted(list(irrational)),
            Product(Power(coeff, Fraction(1, 1)))
        )
        for irrational, coeff in coefficients.items()
        if coeff != 0
    ])
```

Notice that we define `sum_zero = Sum(product_zero)`, but this will actually get
normalised to [the empty sum](https://en.wikipedia.org/wiki/Empty_sum) `Sum()`,
which is the normal form for the zero sum.

### Fixed Point ###

Similar to `Product`, we can keep repeating the normalisation of a `Sum`, until
no more changes are made to its elements:

```python
    result = sorted([
        multiply_products(
            Product(*irrational),
            Product(Power(coeff, Fraction(1, 1)))
        )
        for irrational, coeff in coefficients.items()
        if coeff != 0
    ])
    return result if list(result) == list(products) else Sum(*result)
```

## Conclusion ##

We've defined a `Sum` type, which is a pretty simple and natural step up from
our `Product` type, and allows us to represent more numbers. This `Sum` type has
no redundancy (like `Product`, but unlike `Power`). To normalise these values we
split each of its elements into a rational part and an irrational part, then add
those rational parts as ordinary Python `int`egers.

In the next part we'll introduce one more type by taking *ratios* of these sums!

## Appendixes ##

### Implementation ###

Here's the final implementation of `Sum`, assuming that we have implementations
of `Product` and `Power` defined:

```python
def Sum(*products: Product) -> List[Product]:
    # This Sum has a term for each distinct irrational product, multiplied by
    # the sum of all the rationals it appeared with in the input
    coefficients = irrationals_coefficients(products)
    result = sorted([
        multiply_products(
            Product(*irrational),
            Product(Power(coeff, Fraction(1, 1)))
        )
        for irrational, coeff in coefficients.items()
        if coeff != 0
    ])
    return result if list(result) == list(products) else Sum(*result)

def add_sums(*sums) -> Sum:
    return Sum(*sum(sums, []))

def multiply_sums(*sums) -> Sum:
    return reduce(
        lambda s1, s2: Sum(*[
            multiply_products(p1, p2)
            for p1 in s1
            for p2 in s2
        ]),
        sums,
        sum_one
    )

def split_rationals(product: Product) -> Tuple[Product, Product]:
    if product == product_zero:
        return (product_zero, product_one)
    go = lambda power: reduce(
        multiply_products,
        [Product(Power(base, power(exp))) for base, exp in product],
        product_one
    )
    return (
        go(lambda e: e // 1), # rationals,
        go(lambda e: e %  1)  # irrationals
    )

def irrationals_coefficients(s: Sum):
    result = {}
    for product in s:
        rationals, irrationals = split_rationals(product)
        key = frozenset(irrationals)
        if key not in result:
            result[key] = 0
        result[key] += eval_product_int(rationals)
    return result

def sum_is_rational(s: Sum) -> bool:
    return all(product_is_rational(p) for p in s)

sum_zero = Sum(product_zero)
sum_one = Sum(product_one)
sum_neg = Sum(product_neg)
```

### Property Tests ###

The following tests extend those for `Product`, this time checking various
properties that should hold for our `Sum` type:

```python
@st.composite
def sums(draw, rational=None):
    main = draw(st.lists(
        # All terms of a rational sum are rational; irrational can be a mixture
        products(rational=True if rational == True else None),
        min_size=0,
        max_size=3
    ))
    extra = [draw(products(rational))] if rational == False else []
    normal = Sum(*(main + extra))
    if rational is None:
        return normal
    is_rat = sum_is_rational(normal)
    assume(rational == is_rat)
    return normal


@settings(deadline=None)
@given(st.data())
def test_strategy_sums(data):
    data.draw(sums())

@given(sums(rational=True))
def test_strategy_sums_can_force_rational(s):
    assert sum_is_rational(s)

@given(sums(rational=False))
def test_strategy_sums_can_force_irrational(s):
    assert not sum_is_rational(s)


@given(products())
def test_split_rationals_preserves_terms(p: Product):
    rationals, irrationals = split_rationals(p)

    for base, exp in rationals:
        assert exp.denominator == 1
    for base, exp in irrationals:
        assert exp.denominator > 1
        assert exp.numerator < exp.denominator
    assert multiply_products(rationals, irrationals) == p

@given(products())
def test_coefficients_agree_for_singleton(p: Product):
    coeffs = irrationals_coefficients([p])
    assert len(coeffs) == 1
    rationals, irrationals = split_rationals(p)
    key = frozenset(irrationals)
    assert key in coeffs
    assert eval_product_int(rationals) == coeffs[key]

@given(products(), products())
def test_coefficients_commutes(p1: Product, p2: Product):
    coeffs_12 = irrationals_coefficients([p1, p2])
    coeffs_21 = irrationals_coefficients([p2, p1])
    assert coeffs_12 == coeffs_21

@given(sums())
def test_coefficients_fractions_are_proper(s: Sum):
    for irrationals, coeff in debug(coeffs=irrationals_coefficients(s)).items():
        for base, exp in irrationals:
            assert exp.denominator > 1
            assert exp.numerator < exp.denominator

@settings(deadline=None)
@given(sums())
def test_coefficients_multiply_back_to_original(s: Sum):
    """Use output of irrationals_coefficients to reconstruct a Sum, it should
    normalise to equal the input."""
    terms = []
    for irrationals, coeff in irrationals_coefficients(s).items():
        assume(abs(coeff) < 100) # Avoid crazy-big lists!
        neg = [power_neg] if coeff < 0 else []
        extra = [list(irrationals) + neg] * abs(coeff)
        debug(terms=terms, irrationals=irrationals, coeff=coeff, extra=extra)
        terms += extra
    normal = Sum(*terms)
    debug(total_terms=terms, normal=normal)
    assert normal == s

@given(sums())
def test_normalise_sum_idempotent(s: Sum):
    assert s == Sum(*s)

@given(sums())
def test_multiply_sums_identity(s: Sum):
    assert multiply_sums(s, sum_one) == s, "1 is right identity"
    assert multiply_sums(sum_one, s) == s, "1 is left identity"

@given(sums())
def test_multiply_sums_absorb(s: Sum):
    assert multiply_sums(s, sum_zero) == sum_zero, "0 absorbs on right"
    assert multiply_sums(sum_zero, s) == sum_zero, "0 absorbs on left"

@settings(deadline=90000)
@given(sums(), sums())
def test_multiply_sums_commutative(s1: Sum, s2: Sum):
    result_12 = multiply_sums(s1, s2)
    result_21 = multiply_sums(s2, s1)
    assert result_12 == result_21

@settings(deadline=1000)
@given(sums(), sums(), sums())
def test_multiply_sums_associative(s1: Sum, s2: Sum, s3: Sum):
    result_12 = multiply_sums(s1, s2)
    result_23 = multiply_sums(s2, s3)
    assert multiply_sums(result_12, s3) == multiply_sums(s1, result_23)

@settings(deadline=90000)
@given(sums(), sums(), sums())
def test_multiply_sums_distributive(s1: Sum, s2: Sum, s3: Sum):
    s1_23 = multiply_sums(s1, Sum(*(s2 + s3)))
    s12_13 = Sum(
        *(multiply_sums(s1, s2) + multiply_sums(s1, s3))
    )
    assert s1_23 == s12_13

@given(sums())
def test_negative_sums_cancel(s: Sum):
    assert sum_zero == add_sums(
        s,
        [multiply_products(p, product_neg) for p in s]
    )

@settings(deadline=30000)
@given(products(), st.integers(min_value=1, max_value=10))
def test_like_products_add(p: Product, n: int):
    assert Sum(*([p] * n)) == Sum(multiply_products(p, factorise(n)))
```
