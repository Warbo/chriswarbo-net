---
title: "Rationalising Denominators 3: Sums"
---

Posts in this series:

[Powers](/blog/2024-11-03-rationalising_denominators_1_powers.html)

[Products](/blog/2024-11-05-rationalising_denominators_2_products.html)

[Sums](/blog/2024-11-08-rationalising_denominators_3_sums.html)

[Ratios](/blog/2024-11-13-rationalising_denominators_4_ratios.html)

---

## Introduction ##

In the
[previous post](/blog/2024-11-05-rationalising_denominators_2_products.html)
we generalised our `Power`{.python} type into a `Product`{.python} of
arbitrarily-many powers multiplied together. However, there are values which
can't be represented that way, such as
<abbr title="Power(1, Fraction(0, 1)) + Power(7, Fraction(1, 2))">1+√7</abbr>.
Instead, these can be represented as a *sum* of such `Product`{.python}s, which
we'll define in this post.

## Sums ##

Our initial definition of `Sum`{.python} will begin in a similar way to how we
approached `Product`{.python}, as a list of values:

```python
def Sum(*products: Product) -> List[Product]:
    return list(products)
```

## Normalising ##

Just like our other representations, we want to normalise `Sum`{.python} values,
so that each number is represented in exactly one way.

### Combining multiples ###

The first problem with our representation is that a `Sum`{.python} containing N
copies of the same `Product`{.python} should be the same as a `Sum`{.python}
containing that `Product`{.python} once, multiplied by a factor N. More
generally, a `Sum`{.python} containing `Product`{.python} values of the form
P×M₁ and P×M₂, should be the same as a `Sum`{.python} which instead contains
P×(M₁+M₂).

The key to making this tractable is that a `Sum`{.python} can only have a
rational, whole number of terms. Hence we can begin by restricting our attention
to those factors M₁, M₂, etc. in each `Product`{.python} which are rational,
whole numbers. The following function will split a `Product`{.python} into a
whole part (containing all of the whole exponents) and a fractional/irrational
part (containing any remaining fractions of exponents):

```python
def split_fractionals(product: Product) -> Tuple[Product, Product]:
    if product == product_zero:
        return (product_zero, product_one)
    go = lambda power: reduce(
        multiply_products,
        [Product(Power(base, power(exp))) for base, exp in product.items()],
        product_one
    )
    return (
        go(lambda e: e // 1), # whole exponents
        go(lambda e: e %  1), # fractional exponents
    )
```

Since the part with whole-number exponents is guaranteed to be a whole number,
we can instead `return`{.python} that as a native Python `int`{.python}eger
(which also makes it easier to distinguish which part is which):

```python
def split_fractionals(product: Product) -> Tuple[Product, int]:
    if product == product_zero:
        return (0, product_one)
    go = lambda power: reduce(
        multiply_products,
        [Product(Power(base, power(exp))) for base, exp in product.items()],
        product_one
    )
    return (
        eval_product_int(go(lambda e: e // 1)), # whole exponents
        go(lambda e: e % 1), # fractional exponents
    )
```

Now we can check if any terms have the same fractional parts, and if so combine
their whole-number parts; just like the example of P×M₁ + P×M₂ = P×(M₁+M₂).
We'll use a `Counter`{.python}, provided by Python's `collections`{.python}
module, to add up all of the whole number multiples (AKA coefficients) for each
`Product`{.python} of fractional exponents (AKA irrationals):

```python
def irrationals_coefficients(s: List[Product]) -> Counter:
    result = Counter()
    for p in s:
        count, fractional = split_fractionals(p)
        # The key for our Counter must be hashable, so make a tuple. Sort it, to
        # normalise all permutations of factors into the same order.
        irrational = tuple(sorted(list(Product(*fractional.items()).items())))
        result[irrational] += count  # Counters begin at 0 if not yet assigned
    return result
```

We'll use this `Counter`{.python} as our representation of `Sum`{.python},
rather than the naïve list we started with:

```python
def Sum(*products: Product) -> Counter:
    return irrationals_coefficients(products)
```

### Negatives ###

So far, we've seen that rational, whole number factors are important. So far our
`irrationals_coefficients`{.python} function is assuming that this means factors with
whole-numbered exponents; but we've previously decided to encode negative one as
<abbr title="Power(1, Fraction(1, 2))">√1</abbr>, which is a rational, whole
number that has a fractional exponent!

We'll leave the `split_fractionals`{.python} function as-is, and account for negative one
by post-processing its result in `irrationals_coefficients`{.python} (this is why I named
them differently, one with "fractional" and one with "irrational", since they're
handling subtley different concepts):

```python
def irrationals_coefficients(s: List[Product]) -> Counter:
    result = Counter()
    for p in s:
        count, fractional = split_fractionals(p)
        if fractional.get(1, zero) >= half:
            # Shift a factor of 1^(1/2) from irrationals to rationals, since
            # neg_power is rational
            base, exp = Power(1, fractional.get(1, zero))
            if exp < one and exp >= half:
                count = count * (-1)
                exp -= half
            fractional = fractional | {1: exp}
            if fractional.get(1, None) == zero:
                # Avoid redundant factors appearing in .items()
                del(fractional[1])
        irrational = tuple(sorted(list(Product(*fractional.items()).items())))
        result[irrational] += count
    return result
```

### Skipping zeroes ###

We can add a final tweak to the end of `irrationals_coefficients`{.python}, to remove any
elements that end up with a count of zero (otherwise `Counter`{.python} may keep them
around):

```python
        result[irrational] += count
        if result[irrational] == 0:
            del(result[irrational])
    return result
```

Notice that with this change, our definition of `sum_zero = Sum(product_zero)`{.python}
will normalise to
[the empty sum](https://en.wikipedia.org/wiki/Empty_sum), as if we had defined
it as `Sum()`{.python} instead!

### Normalising recursively ###

To simplify later steps, we'll implement a couple of passes through the
structure of a `Sum`{.python}; allowing smaller parts to be normalised in isolation.
First we'll need a way to compare two `Sum`{.python} values, to see if they've reached a
fixed-point of our normalisation process. We can do that by putting their
contents into a sequence, and sorting that into ascending order:

```python
def s_items(coefficients):
    return tuple(sorted(coefficients.items()))

def sums_are_same(x, y):
    return s_items(x) == s_items(y)
```

Next we'll defining a `normalise_sum`{.python} function to hold our subsequent processing
and call it from the top-level `Sum`{.python} constructor:

```python
def Sum(*products: Product) -> Counter:
    return normalise_sum(irrationals_coefficients(products))

def normalise_sum(s: Sum) -> Sum:
    if len(s) < 2:
        # If we have 0 or 1 terms, there's no summing to do.
        return s
    # Other normalisation steps can go here
    return s
```

#### Holding out terms ####

One way to isolate part of a `Sum`{.python} is to remove some terms, normalise without
them, then add them back after. That last part requires an addition function for
`Sum`{.python}s, which we can implement by accumulating the contents of their `Counter`{.python}s:

```python
def add_sums(*sums) -> Sum:
    result = Counter()
    for s in sums:
        # Annoyingly, we can't just result += s, since that ignores negatives
        for k in s:
            result[k] += s[k]
    return normalise_sum(result)
```

Then we can add the following loop into `normalise_sum`{.python}, which will hold out
each term individually and see if the remaining terms normalise any more without
it: if so, add the held-out term to that new result and recurse; otherwise carry
on to the next iteration. This combination of looping and recursing ensures that
we'll try to normalise every distinct subset of terms before giving up.

```python
    # Try holding-out each term at a time. Recursion will take care of trying
    # each combination of held-out terms.
    for term, count in s.items():
        # Make a copy of s with this term deleted
        init = Counter(s)
        del(init[term])

        # See if normalising that subset changes its contents
        norm = normalise_sum(init)
        if not sums_are_same(init, norm):
            # Normalising the subset made progress; add it to the held-out term
            signed = Product(power_one if count >= 0 else power_neg, *term)
            return add_sums(norm, Counter({term: count}))
```

#### Holding out common factors ####

The second way we can hold-out part of a `Sum`{.python} is to divide-through by its
common factors, normalise what's left, then multiply the result by the held-out
factors. This is especially powerful thanks to the holding-out of terms we just
implemented above; since we're more likely to find common factors in a subset of
terms.

We'll start with a multiplication function for `Sum`{.python}s; although that's easier to
implement for our original list-of-`Product`{.python} representation, so we'll include a
function to convert into that format as well:

```python
def _sum_to_products(coefficients):
    return _sort([
        Product(
            Power(abs(coeff), one),
            power_neg if coeff < 0 else power_one,
            *irrational
        )
        for irrational, coeff in coefficients.items()
        if coeff != 0
    ])

def _sort(s: Sum) -> Sum:
    return sorted(list(s), key=lambda p: sorted(p.items()))

def multiply_sums(*sums: Sum) -> Sum:
    # Skip multiplication by 1
    remaining = [s for s in sums if s != sum_one]
    if len(remaining) == 0:
        return sum_one  # Empty product

    first, *rest = remaining
    # Short-circuit if we're only multiplying one term
    if len(rest) == 0:
        return first
    return reduce(
        lambda s1, s2: Sum(*[
            multiply_products(p1, p2)
            for p1 in _sum_to_products(s1)
            for p2 in _sum_to_products(s2)
        ]),
        rest,
        first
    )
```

Next we'll need to identify common factors:

```python
def common_factors_of_sum(s: Sum) -> Product:
    if s == sum_zero:
        return product_one
    first, *rest = _sum_to_products(s)
    found = reduce(
        lambda common, product: {
            base: min(common[base], augmented[base])
            for base in common
            for augmented in [{1: one} | product]
            if base in augmented
        },
        rest,
        {
            base: exp
            for base, exp in ({1: one} | first).items()
        }
    )
    # Avoid factors of 1^0 AKA 1^1
    if found.get(1, None) in [zero, one]: del(found[1])
    return found
```

And we need a way to "divide-through" by a common factor. Since our numeric
representation doesn't support arbitrary divisions yet, we'll make a specialised
function that relies on the divisor being a factor. To avoid infinite recursion
we'll give this function a `bool`{.python}ean parameter called `raw`{.python}, to indicate when we
don't want it to attempt further normalisation:

```python
def remove_common_factor(s: Sum, p: Power, raw=False) -> Sum:
    if p == power_one:
        return s  # Divide by 1 is a no-op
    base, exp = p
    old_products = _sum_to_products(s)

    # Check our precondition, that p is a common factor of the terms in s
    has_base = all(base in product for product in old_products)
    if not (has_base or base == 1):
        raise AssertionError(
            f"remove_common_factor({s}, {p}, {raw})"
        )

    coeffs = irrationals_coefficients([
        Product(*(
            [
                Power(b, e - exp if b == base else e) for b, e in product.items()
            ] + ([] if base != 1 or base in product else [Power(1, one - exp)])
        ))
        for product in old_products
    ])
    return coeffs if raw else normalise_sum(coeffs)
```

Finally we can use these in our `normalise_sum`{.python} function to hold-out common
factors:

```python
    # Try holding-out each common factor at a time. Recursion will take care of
    # trying each combination of held-out factors.
    factors = common_factors_of_sum(s)
    factors_list = factors.items()
    for (base, exp) in factors_list:
        rest = remove_common_factor(s, (base, exp), raw=True)
        norm = normalise_sum(rest)
        if not sums_are_same(norm, rest):
            return multiply_sums(norm, Sum(Product(Power(base, exp))))
```

#### Allowing remainders ####

The above implementation of `common_factors_of_sum`{.python} is actually quite limited,
since it will only accept factors divide *exactly* into every element of a
`Sum`{.python}. However, it will sometimes be useful to hold-out factors which leave a
(whole, rational) remainder after division. To support that, we'll need a way to
split the rational part off a `Sum`{.python}:

```python
def _split_rational(s: Sum) -> Tuple[Sum, int]:
    coeffs = Counter(s)
    rational = coeffs[tuple()]
    del(coeffs[tuple()])
    return (coeffs, rational)
```

Now we can handle the rational part separately when looking for common factors:

```python
def common_factors_of_sum(s: Sum, allow_remainder=False) -> (Product, int):
    """Returns a Product whose Powers appear in every term of the given Sum.
    This takes into account bases with differing exponents, by returning the
    smallest. If allow_remainder, rationals (Powers of 1^0 or 1^0.5) are allowed
    to leave a remainder, which is returned as the second part of the tuple."""
    if allow_remainder:
        to_factor_coeffs, const = _split_rational(s)
    else:
        to_factor_coeffs = s
        const = 0

    if to_factor_coeffs == sum_zero:
        if const:
            return (
                multiply_products(
                    Product(Power(abs(const), one)),
                    product_one if const >= 0 else product_neg,
                ),
                0
            )
        else:
            return (product_one, 0)
    first, *rest = _sum_to_products(to_factor_coeffs)
    found = reduce(
        lambda common, product: {
            base: min(common[base], augmented[base])
            for base in common
            for augmented in [{1: one} | product]
            if base in augmented
        },
        rest,
        {
            base: exp
            for base, exp in ({1: one} | first).items()
        }
    )
    # Avoid factors of 1^0 AKA 1^1
    if found.get(1, None) in [zero, one]: del(found[1])

    if const:
        # If we held out the rational part, see if we can divide it by found
        factored_coeffs, remainder = _split_rational(
            irrationals_coefficients([found])
        )
        if factored_coeffs == sum_zero:
            # No irrational part, so we can divide our held-out const
            return (found, const % remainder)
    return (found, const)
```

Here's the corresponding change in `normalise_sum`{.python}:

```python
    # Try holding-out each common factor at a time. Recursion will take care of
    # trying each combination of held-out factors.
    factors, remainder = common_factors_of_sum(s, allow_remainder=True)
    reduced = _subtract_int_from_sum(s, remainder)
    factors_list = factors.items()
    for (base, exp) in factors_list:
        rest = remove_common_factor(reduced, (base, exp), raw=True)
        norm = normalise_sum(rest)
        if not sums_are_same(norm, rest):
            return add_sums(
                Sum(multiply_products(
                    Product(Power(abs(remainder), 1)),
                    product_neg if remainder < 0 else product_one
                )),
                multiply_sums(norm, Sum(Product(Power(base, exp))))
            )
```

## Conclusion ##

We've defined a `Sum`{.python} type, which is a natural progression from our
`Product`{.python} type, and allows us to represent more numbers. Our
representation normalises many redundant aspects of a `Sum`{.python}, including
permutations of the order of terms, skipping terms which are zero, combining
multiple copies of the same term (including those appearing with extra factors),
and having negative multiples cancel-out positive ones.

The normalisation of `Sum`{.python} is actually overkill for the techniques seen
in this blog post, but will prove invaluable when we implement *roots of unity*
in a subsequent installment.

In [the next part](/blog/2024-11-13-rationalising_denominators_4_ratios.html)
we'll introduce one more type by taking *ratios* of these sums!

## Appendixes ##

### Implementation ###

Here's the final implementation of `Sum`{.python}, assuming that we have
implementations of `Product`{.python} and `Power`{.python} defined:

```python
def Sum(*products: Product) -> Counter:
    """This Sum has a term for each distinct irrational product, multiplied by
    the sum of all the rationals it appeared with in the input."""
    # Combine coefficients of like terms
    return normalise_sum(irrationals_coefficients(products))

def _sort(s: Sum) -> Sum:
    return sorted(list(s), key=lambda p: sorted(p.items()))

def items(s: Sum) -> List[tuple[Power]]:
    """Convert the Products in a Sum into lists for comparison."""
    return sorted(map(p_items, s))

def p_items(p: Product):
    return sorted(p.items())

def s_items(coefficients):
    return tuple(sorted(coefficients.items()))

def normalise_sum(s: Sum) -> Sum:
    if len(s) < 2:
        # If we have 0 or 1 terms, there's no summing to do.
        return s

    for coeff in s.values():
        check_size(coeff)

    # Try holding-out each common factor at a time. Recursion will take care of
    # trying each combination of held-out factors.
    factors, remainder = common_factors_of_sum(s, allow_remainder=True)
    reduced = _subtract_int_from_sum(s, remainder)
    factors_list = factors.items()
    for (base, exp) in factors_list:
        rest = remove_common_factor(reduced, (base, exp), raw=True)
        norm = normalise_sum(rest)
        if not sums_are_same(norm, rest):
            return add_sums(
                Sum(multiply_products(
                    Product(Power(abs(remainder), 1)),
                    product_neg if remainder < 0 else product_one
                )),
                multiply_sums(norm, Sum(Product(Power(base, exp))))
            )

    # Try holding-out each term at a time. Recursion will take care of trying
    # each combination of held-out terms.
    for term, count in s.items():
        init = Counter(s)
        del(init[term])
        norm = normalise_sum(init)
        if not sums_are_same(init, norm):
            signed = Product(power_one if count >= 0 else power_neg, *term)
            return add_sums(norm, Counter({term: count}))

    # Otherwise, we're finished; just remove anything that's become zero
    for k in [k for k, v in s.items() if v == zero]:
        del(s[k])
    return s

def sums_are_same(x, y):
    return s_items(x) == s_items(y)


def _subtract_int_from_sum(s: Sum, i: int):
    result = Counter(s)
    result[tuple()] -= i
    if result[tuple()] == 0:
        del(result[tuple()])
    return result

def _split_rational(s: Sum) -> Tuple[Sum, int]:
    coeffs = Counter(s)
    rational = coeffs[tuple()]
    del(coeffs[tuple()])
    return (coeffs, rational)

def _sum_to_products(coefficients):
    return _sort([
        Product(
            Power(abs(coeff), one),
            power_neg if coeff < 0 else power_one,
            *irrational
        )
        for irrational, coeff in coefficients.items()
        if coeff != 0
    ])


def add_sums(*sums) -> Sum:
    result = Counter()
    for s in sums:
        # Annoyingly, we can't just result += s, since that ignores negatives
        for k in s:
            result[k] += s[k]
    return normalise_sum(result)

def multiply_sums(*sums: Sum) -> Sum:
    # Skip multiplication by 1
    remaining = [s for s in sums if s != sum_one]
    if len(remaining) == 0:
        # Empty product
        return sum_one

    first, *rest = remaining
    # Short-circuit if we're only multiplying one term
    if len(rest) == 0:
        return first
    return reduce(
        lambda s1, s2: Sum(*[
            multiply_products(p1, p2)
            for p1 in _sum_to_products(s1)
            for p2 in _sum_to_products(s2)
        ]),
        rest,
        first
    )

def split_fractionals(product: Product) -> Tuple[Product, int]:
    if product == product_zero:
        return (0, product_one)
    go = lambda power: reduce(
        multiply_products,
        [Product(Power(base, power(exp))) for base, exp in product.items()],
        product_one
    )
    return (
        eval_product_int(go(lambda e: e // 1)), # whole exponents
        go(lambda e: e % 1), # fractional exponents
    )

def irrationals_coefficients(s: List[Product]) -> Counter:
    result = Counter()
    for p in s:
        count, fractional = split_fractionals(p)
        if fractional.get(1, zero) >= half:
            # Shift a factor of 1^(1/2) from irrationals to rationals, since
            # neg_power is rational
            base, exp = Power(1, fractional.get(1, zero))
            if exp < one and exp >= half:
                count = count * (-1)
                exp -= half
            fractional = fractional | {1: exp}
            if fractional.get(1, None) == zero:
                # Avoid redundant factors appearing in .items()
                del(fractional[1])
        irrational = tuple(sorted(list(Product(*fractional.items()).items())))
        result[irrational] += count
        if result[irrational] == 0:
            del(result[irrational])
    return result
irrationals_coefficients = cached(
    irrationals_coefficients,
    to_key=_key_products
)

def is_zero(s: Sum) -> bool:
    for k in [k for k in s if s[k] == 0]:
        del(s[k])
    return s == sum_zero

def common_factors_of_sum(s: Sum, allow_remainder=False) -> (dict, int):
    """Returns a Product whose Powers appear in every term of the given Sum.
    This takes into account bases with differing exponents, by returning the
    smallest. If allow_remainder, rationals (Powers of 1^0 or 1^0.5) are allowed
    to leave a remainder, which is returned as the second part of the tuple."""
    if allow_remainder:
        to_factor_coeffs, const = _split_rational(s)
    else:
        to_factor_coeffs = s
        const = 0

    if to_factor_coeffs == sum_zero:
        if const:
            return (
                multiply_products(
                    Product(Power(abs(const), one)),
                    product_one if const >= 0 else product_neg,
                ),
                0
            )
        else:
            return (product_one, 0)
    first, *rest = _sum_to_products(to_factor_coeffs)
    found = reduce(
        lambda common, product: {
            base: min(common[base], augmented[base])
            for base in common
            for augmented in [{1: one} | product]
            if base in augmented
        },
        rest,
        {
            base: exp
            for base, exp in ({1: one} | first).items()
        }
    )
    # Avoid factors of 1^0 AKA 1^1
    if found.get(1, None) in [zero, one]: del(found[1])

    if const:
        # If we held out the rational part, see if we can divide it by found
        factored_coeffs, remainder = _split_rational(
            irrationals_coefficients([found])
        )
        if factored_coeffs == sum_zero:
            # No irrational part, so we can divide our held-out const
            return (found, const % remainder)
    return (found, const)

def remove_common_product(s: Sum, p: Product, raw=False) -> Sum:
    return reduce(
        lambda s2, p2: remove_common_factor(
            s2,
            p2,
            raw=raw
        ),
        p.items(),
        s
    )

def remove_common_factor(s: Sum, p: Power, raw=False) -> Sum:
    if p == power_one:
        return s
    base, exp = p
    old_products = _sum_to_products(s)
    has_base = all(base in product for product in old_products)
    if not (has_base or base == 1):
        raise AssertionError(
            f"remove_common_factor({s}, {p}, {raw})"
        )

    coeffs = irrationals_coefficients([
        Product(*(
            [
                Power(b, e - exp if b == base else e) for b, e in product.items()
            ] + ([] if base != 1 or base in product else [Power(1, one - exp)])
        ))
        for product in old_products
    ])
    return coeffs if raw else normalise_sum(coeffs)

def sum_is_rational(s: Sum) -> bool:
    if s == sum_zero:
        return True
    if len(s) == 1:
        return tuple() in s
    return False

def evaluate_sum(s: Sum) -> float:
    return sum([evaluate_product(p) for p in _sum_to_products(s)], 0.0)

sum_zero = Counter()
sum_one = Counter([tuple()])
sum_neg = Counter({tuple(): -1})
```

### Property tests ###

The following tests extend those for `Product`{.python}, this time checking various
properties that should hold for our `Sum`{.python} type:

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
