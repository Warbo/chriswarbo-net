---
title: "Rationalising Denominators 2: Products"
---

Posts in this series:

[Powers](/blog/2024-11-03-rationalising_denominators_1_powers.html)

[Products](/unfinished/rationalising_denominators_2_products.html)

---

[Last time](/blog/2024-11-03-rationalising_denominators_1_powers.html) we used
Python's `int` and `Fraction` types to implement a new type called `Power`, to
represent [nth roots](https://en.wikipedia.org/wiki/Nth_root).

We implemented a few normalisation steps, to avoid redundant values like
`Power(-1, Fraction(2, 1))` (negative one squared, which is just one),
`Power(1, Fraction(10, 1))` (one to the power of ten, which is also just one)
and `Power(0, Fraction(3, 1))` (the cube of zero, which is just zero).

We were still left with some redundancy though, like `Power(4, Fraction(1, 1))`
(four to the power one) and `Power(2, Fraction(2, 1))` (two squared) both being
the number four; or, conversely, `Power(2, Fraction(1, 1))` (two to the power
one) and `Power(4, Fraction(1, 2))` (square root of four) both being the number
two.

This time we'll use `Power` to implement another type, called `Product`, which
completely avoids these redundancies.

## Products ##

A product is a list of values multiplied together. We'll can define a `Product`
type in Python, whose elements are `Power` values:

```python
def Product(*powers: Power) -> List[Power]:
    return list(powers)
```

This simple implementation can represent all the same values as `Power` (by
using a list with a single value), but it also lets us represent other values
such as `Product(Power(3, Fraction(1, 1)), Power(2, Fraction(1, 2)))` (three
times the square root of two).

Here are some useful constants for this type:

```python
product_zero = Product(power_zero)
product_one = Product(power_one)
product_neg = Product(power_neg)
```

### Multiplication ###

Since a `Product` represents a multiplication of its elements, we can multiply
`Product` values by concatenating their lists:

```python
def multiply_products(*ps: Product) -> Product:
    return Product(*(sum(ps, [])))
```

## Normalisation ##

There is plenty of redundancy in this simple definition, so we'll build up a
normalisation algorithm which consolidates such values to a single
representation.

### Permutations ###

Since our multiplication is commutative (for the moment), changing the order of
elements in the list does not affect the result. We can solve this quite easily
by sorting the values (since each `Power` is a tuple, Python will use a
lexicographic ordering of the fields; with a numerical ordering for the numbers
inside a field; however the the actual order doesn't matter, so long as *some*
permutation of our list is used as the "normal" form):

```python
def Product(*powers: Power) -> List[Power]:
    return sorted(list(powers))
```

### Repeated Bases ###

A `Product` of `Powers` with the same `base` (like
`Product(Power(5, Fraction(1, 2)), Power(5, Fraction(3, 4)))`)
[is equivalent](https://en.wikipedia.org/wiki/Exponentiation#Identities_and_properties)
to a single `Power` with the same base, but sum of those `exponent`s (in this
case `Power(5, Fraction(5, 4))`). We'll prefer the latter, and normalise our
`Product` values to that form. We'll implement this by summing the `exponent`s
of given `Power`s in a dictionary, keyed on their `base`; then converting the
final entries into the resulting list of `Power`s:

```python
def Product(*powers: Power) -> List[Power]:
    combined = {}
    for base, exp in powers:
        if base in combined:
            combined[base] += exp
        else:
            combined[base] = exp
    return [Power(base, exp) for base, exp in sorted(combined.items())]
```

### Absorbing Zero ###

All products which contain zero are equivalent (since zero is the
[absorbing value](https://en.wikipedia.org/wiki/Absorbing_element) for
multiplication). The simplest of these is the product which *only* contains
zero, so we'll normalise the others to that:

```python
        # If any element is zero, the whole Product is zero
        if base == 0:
            return [power_zero]
```

### Discarding Ones ###

The number one is the
[identity value](https://en.wikipedia.org/wiki/Identity_element) for
multiplication, so its presence in a product does not alter the result. Hence
the normalisation of a `Product` should remove any such `Power`. Thankfully, we
can rely on `Power` to normalise such values to `Power(1, Fraction(0, 1))`; and
indeed we can spot them by checking if their `exponent` is zero:

```python
    return [
        Power(base, exp) for base, exp in sorted(combined.items()) if exp != 0
    ]
```

Note that, if *every* `Power` given to a `Product` is one, then the result will
be the [empty product](https://en.wikipedia.org/wiki/Empty_product); and this is
the normal form for the `Product` representing the number one!

### Fixed Points ###

Relying on the normalisation of `Power` makes our life easier, but we may still
miss redundancies due to each `Power` being normalised individually. In
particular, consider a value like `Product(power_neg, power_neg)`: the repeated
`base` of `-1` will be `combined` into a dictionary like
`{ -1: Fraction(2, 1) }`. Since that `exponent` is not `0`, our previous check
won't skip it, and we'll return a list containing the element
`Power(-1, Fraction(2, 1))`, which normalises to `Power(1, Fraction(0, 1))`.
Hence double-negatives will lead to redundant factors of one in a `Product`.

This is unfortunate, since the normalisation performed by `Power` has replaced
the `exponent` with zero; so our check *could* skip it, if we perform it later.
There are many ways we could adjust our code to fix this, but one particularly
simple approach is to *repeat* the normalisation steps on our result, until the
value no longer changes (i.e. until we reach a
[fixed point](https://en.wikipedia.org/wiki/Fixed_point_(mathematics))):

```python
    result = [
        Power(base, exp) for base, exp in sorted(combined.items()) if exp != 0
    ]
    return result if list(result) == list(powers) else Product(*result)
```

### Factorising ###

Now we confront the main source of redundancy in both `Product` and `Power`:
that different sets of powers can multiply to the same result; and indeed that
raising different `base` values to appropriate `exponent`s can also produce the
same result.

We will avoid this redundancy by only allowing elements of a `Product` to have
`base` values which are [prime](https://en.wikipedia.org/wiki/Prime_number), or
the special values `0` (the only factor of zero), `-1` (a factor of every
negative number) and `1` (if raised to a fractional power).

To implement this normalisation, we need some helper functions to
[factorise](https://en.wikipedia.org/wiki/Integer_factorization) `base` values.
There are many algorithms we could use, and the details aren't too important for
our purposes; you can expand the following box to see the implementation I've
used.

<details class="odd">
<summary>Implementation of `factorise`</summary>

Factorising can be slow, and was the bottleneck in my initial experiments. The
following function uses a few tricks to speed up calculations, compared to a
naive approach:

 - The `lru_cache` decorator gives the function a
   [memo table](https://en.wikipedia.org/wiki/Memoization), so results which
   have previously been calculated can be quickly re-used.
 - During startup there is a one-time cost to calculate the primes up to 1000,
   which are used to factor small numbers.
 - If we have to calculate the factors of some large number, we use a "wheel
   factorisation" algorithm, which is at least faster than brute-force search!

```python
from functools import lru_cache

def factorise() -> Callable[[int], Product]:
    # Precompute primes up to 1000 using Sieve of Eratosthenes
    sieve = [True] * 1001
    sieve[0] = sieve[1] = False
    for i in range(2, 32):  # sqrt(1000) ≈ 31.6
        if sieve[i]:
            for j in range(i * i, 1001, i):
                sieve[j] = False
    SMALL_PRIMES = [i for i in range(1001) if sieve[i]]
    del(sieve)

    # Slow, but works for arbitrary n
    def wheel_factorise(n: int) -> Product:
        factors = []

        # Handle 2, 3 and 5 separately for efficiency
        for p in (2, 3, 5):
            if n % p == 0:
                count = 0
                while n % p == 0:
                    count += 1
                    n //= p
                factors.append(Power(p, Fraction(count, 1)))

        # Wheel increments for numbers coprime to 2, 3 and 5
        increments = [4, 2, 4, 2, 4, 6, 2, 6]
        i = 0
        f = 7

        while f * f <= n:
            if n % f == 0:
                count = 0
                while n % f == 0:
                    count += 1
                    n //= f
                factors.append(Power(f, Fraction(count, 1)))
            f += increments[i]
            i = (i + 1) % 8

        if n > 1:
            factors.append((n, 1))

        return factors

    @lru_cache(maxsize=1024)
    def factorise(n: int) -> Product:
        if n == 0:
            return [power_zero]
        if n == 1:
            return []
        if n < 0:
            # Combine -1 with factors of the absolute value
            return [power_neg] + factorise(-n)
        if n > 1000:
            # Fallback method for large numbers
            return wheel_factorise(n)
        else:
            # Small numbers can use precomputed primes
            factors = []
            for p in SMALL_PRIMES:
                if p * p > n:
                    if n > 1:
                        factors.append(Power(n, Fraction(1, 1)))
                    break

                if n % p == 0:
                    count = 0
                    while n % p == 0:
                        count += 1
                        n //= p
                    factors.append(Power(p, Fraction(count, 1)))
            return factors
    return lambda n: sorted(factorise(n))
factorise = factorise()
```

</details>

We use that `factorise` function to implement the following `split_power`
function, which adjusts factors of a `base` to account for the given `exponent`:

```python
def split_power(power: Power) -> Product:
    base, exp = power
    return [Power(b, exp * e) for b, e in factorise(base)]
```

This can then be used to normalise the elements of a `Product`:

```python
def Product(*powers: Power) -> List[Power]:
    combined = {}
    for power in powers:
        for base, exp in split_power(power):
            # If any element is zero, the whole Product is zero
            if base == 0:
                return [power_zero]
            if base in combined:
                combined[base] += exp
            else:
                combined[base] = exp
    result = [
        Power(base, exp) for base, exp in sorted(combined.items()) if exp != 0
    ]
    return result if list(result) == list(powers) else Product(*result)
```

The redundancy in our earlier examples is now avoided, since any `Power` with
`base` 4 will be replaced by an equivalent with `base` 2:

 - `Product(Power(4, Fraction(1, 1)))` (four to the power one) will normalise to
   `Product(Power(2, Fraction(2, 1)))` (two squared)
 - `Power(4, Fraction(1, 2))` (square root of four) will normalise to
   `Power(2, Fraction(1, 1))` (two to the power one)

Note that we don't perform this factorising in `Power`, since it may require a
`Product` of multiple elements. For example, normalising
`Product(Power(18, Fraction(2, 1)))` (eighteen squared) produces:

```python
Product(Power(2, Fraction(2, 1)), Power(3, Fraction(4, 1)))
```

### Roots Of Unity ###

Our final normalisation step concerns fractional powers of one. Consider the
cube root `Power(1, Fraction(1, 3))`: this represents a value which, when
multiplied by itself three times, results in one. The number one itself has this
property, since 1×1×1=1, but it's not the *only* number to do so. In particular,
[roots of unity](https://en.wikipedia.org/wiki/Root_of_unity) have this
property, whilst being distinct from the usual
[number line](https://en.wikipedia.org/wiki/Number_line). Roots of unity will
come in handy later, so we will not replace them with one (this is also why
`Power` uses modulo one to normalise values with `base` one).

There is still some redundant structure in these fractional powers of one, since
any with an `exponent` greater than `Fraction(1, 2)` is equivalent to the
negative of some `Power` with `exponent` less than `Fraction(1, 2)`. We can
account for this by having `split_power` replace factors of
`Power(1, Fraction(1, 2))` with `power_neg`:

```python
def split_power(power: Power) -> Product:
    base, exp = power
    if base == 1:
        if exp >= Fraction(1, 2):
            # Half-way-round the roots of unity, we can split off a power of -1
            return [power_neg] + split_power(
                Power(base, exp - Fraction(1, 2))
            )
    return [Power(b, exp * e) for b, e in factorise(base)]
```

## Extra Helpers ##

We'll be needing the following helper functions, so might as well define them
here alongside `Product`:

```python
def eval_product_int(p: Product) -> int:
    result = 1
    for base, exp in p:
        assert exp.denominator == 1, f"Can't eval {base}^{exp} as int"
        result *= base**exp.numerator
    return result

def product_is_rational(p: Product) -> bool:
    return all(exp.denominator == 1 for _, exp in p)
```

## Conclusion ##

We've extended our representation of numbers from `Power`, which was limited and
suffered from redundancy; to `Product`, which can represent more values and does
not have any redundancies.

Normalising this `Product` representation has exposed some structure that will
be useful in future installments: namely the use of *prime factors* and *roots
of unity*.

## Appendixen ##

### Implementation ###

Here's the implementation of `Product`, assuming that we have an implementation
of `Power` defined. We also include a helper function `product_is_rational`,
which is used in the tests below:

```python
def Product(*powers: Power) -> List[Power]:
    combined = {}
    for power in powers:
        for base, exp in split_power(power):
            if base == 0:
                # If any element is zero, the whole Product is zero
                return [power_zero]
            if base in combined:
                combined[base] += exp
            else:
                combined[base] = exp
    result = [
        Power(base, exp) for base, exp in sorted(combined.items()) if exp != 0
    ]
    return result if list(result) == list(powers) else Product(*result)

def multiply_products(*ps: Product) -> Product:
    return Product(*(sum(ps, [])))

def eval_product_int(p: Product) -> int:
    result = 1
    for base, exp in p:
        assert exp.denominator == 1, f"Can't eval {base}^{exp} as int"
        result *= base**exp.numerator
    return result

def product_is_rational(p: Product) -> bool:
    return all(exp.denominator == 1 for _, exp in p)

def split_power(power: Power) -> Product:
    base, exp = power
    if base == 1:
        # Half-way-round the roots of unity, we can split off a power of -1
        if exp >= Fraction(1, 2):
            return [power_neg] + split_power(
                Power(base, exp - Fraction(1, 2))
            )
    return [Power(b, exp * e) for b, e in factorise(base)]

def factorise() -> Callable[[int], Product]:
    # Precompute primes up to 1000 using Sieve of Eratosthenes
    sieve = [True] * 1001
    sieve[0] = sieve[1] = False
    for i in range(2, 32):  # sqrt(1000) ≈ 31.6
        if sieve[i]:
            for j in range(i * i, 1001, i):
                sieve[j] = False
    SMALL_PRIMES = [i for i in range(1001) if sieve[i]]
    del(sieve)

    # Slow, but works for arbitrary n
    def wheel_factorise(n: int) -> Product:
        factors = []

        # Handle 2, 3 and 5 separately for efficiency
        for p in (2, 3, 5):
            if n % p == 0:
                count = 0
                while n % p == 0:
                    count += 1
                    n //= p
                factors.append(Power(p, Fraction(count, 1)))

        # Wheel increments for numbers coprime to 2, 3 and 5
        increments = [4, 2, 4, 2, 4, 6, 2, 6]
        i = 0
        f = 7

        while f * f <= n:
            if n % f == 0:
                count = 0
                while n % f == 0:
                    count += 1
                    n //= f
                factors.append(Power(f, Fraction(count, 1)))
            f += increments[i]
            i = (i + 1) % 8

        if n > 1:
            factors.append((n, 1))

        return factors

    @lru_cache(maxsize=1024)
    def factorise(n: int) -> Product:
        if n == 0:
            return [power_zero]
        if n == 1:
            return []
        if n < 0:
            # Combine (-1, 1) with positive factorization
            return [power_neg] + factorise(-n)
        if n > 1000:
            # Fallback method for large numbers
            return wheel_factorise(n)
        else:
            # Small numbers can use precomputed primes
            factors = []
            for p in SMALL_PRIMES:
                if p * p > n:
                    if n > 1:
                        factors.append(Power(n, Fraction(1, 1)))
                    break

                if n % p == 0:
                    count = 0
                    while n % p == 0:
                        count += 1
                        n //= p
                    factors.append(Power(p, Fraction(count, 1)))
            return factors
    return lambda n: sorted(factorise(n))
factorise = factorise()

product_zero = Product(power_zero)
product_one = Product(power_one)
product_neg = Product(power_neg)
```

### Property Tests ###

The following test suite uses [Hypothesis](https://hypothesis.readthedocs.io)
and [Pytest](https://pytest.org) to check various properties that should hold
for our `Product` type:

```python
import pytest
from hypothesis import assume, given, settings, strategies as st, note
from functools import reduce
from math import gcd

@lru_cache(maxsize=1024)
def is_prime(n: int) -> bool:
    if n == 0:
        return False
    factors = factorise(n)
    return len(factors) == 1 and factors[0][1] == 1

# Test strategies
@st.composite
def primes(draw):
    n = draw(st.integers(min_value=2, max_value=7))
    return next(p for p in range(n, n+5) if is_prime(p))

@st.composite
def prime_factors(draw, rational=None):
    base = draw(primes())
    num = draw(st.integers(
        min_value={
            True: 1,
            False: 0,
            None: 0
        }[rational],
        max_value=4
    ))
    den = draw(
        st.integers(
            min_value={
                True: 1,
                False: 2,
                None: 1
            }[rational],
            max_value={
                True: 1,
                False: 2,
                None: 1
            }[rational]
        ).filter(lambda d: (d != num) if rational == False else True)
    )
    normalised = list(filter(
        {
            True: lambda x: product_is_rational([x]),
            False: lambda x: not product_is_rational([x]),
            None: lambda x: True
        }[rational],
        split_power(Power(base, Fraction(num, den)))
    ))
    assume(len(normalised) > 0)
    return normalised[0]

def powers(rational=None):
    just_powers = prime_factors(rational)
    with_negatives = st.one_of(st.just(power_neg), just_powers)
    return {
        False: just_powers,
        True: with_negatives,
        None: with_negatives
    }[rational]

@st.composite
def products(draw, rational=None):
    main = draw(st.lists(
        powers(rational={
            # Rational products need entirely rational powers (when in normal
            # form), but irrational products don't need *all* of their powers
            # to be irrational
            True: True,
            False: None,
            None: None
        }[rational]),
        min_size=0,
        max_size=3
    ))
    # We make it more likely for our product to be irrational by appending an
    # extra power that's definitely irrational. This may end get cancelled-out
    # by something we already have, so we still need a post-filter; we just do
    # this to prevent a lot of discarding in the post-filter.
    extra = [draw(powers(rational))] if rational == False else []
    normal = Product(*(main + extra))
    if rational == None:
        return normal
    is_rat = product_is_rational(normal)
    assume(rational == is_rat)
    return normal


@given(st.data())
def test_strategy_products(data):
    data.draw(products())

@given(products(rational=True))
def test_strategy_products_can_force_rational(p):
    assert product_is_rational(p)

@given(products(rational=False))
def test_strategy_products_can_force_irrational(p):
    assert not product_is_rational(p)


@given(st.integers(min_value=2, max_value=1000))
def test_prime_factors_are_prime(n: int):
    prime_factors = factorise(n)
    for base, _ in prime_factors:
        assert is_prime(base)

@given(st.integers(min_value=2, max_value=1000))
def test_prime_factors_multiply_to_input(n: int):
    prime_factors = factorise(n)
    product = eval_product_int(prime_factors)
    debug(prime_factors=prime_factors, product=product)
    assert product == n

def whole_products():
    return products().map(
        lambda xs: Product(*[
            Power(base, Fraction(exp.numerator)) for base, exp in xs
        ])
    )

@given(products())
def test_normalise_product_idempotent(p: Product):
    assert p == Product(*p)

@given(st.lists(
    st.integers(min_value=-1000, max_value=1000),
    min_size=0,
    max_size=3
))
def test_normalise_product_composites(l: List[int]):
    normal = Product(*[Power(base, Fraction(1)) for base in l])
    for base, exp in normal:
        assert exp.denominator == 1, f"{normal} contains fractional power {exp}"

@given(products(), products())
def test_normalise_product_absorbs(pre: Product, suf: Product):
    assert Product(*(pre + [power_zero] + suf)) == product_zero


@given(whole_products(), whole_products())
def test_multiply_products_agrees_with_evaluated(p1, p2):
    i1 = eval_product_int(p1)
    i2 = eval_product_int(p2)
    i3 = eval_product_int(multiply_products(p1, p2))
    assert i3 == i1 * i2

@given(products())
def test_multiply_products_identity(p: Product):
    assert multiply_products(p, product_one) == p, "1 is right identity"
    assert multiply_products(product_one, p) == p, "1 is left identity"

@given(products(), products())
def test_multiply_products_commutative(p1: Product, p2: Product):
    result_12 = multiply_products(p1, p2)
    result_21 = multiply_products(p2, p1)
    assert result_12 == result_21

@given(products(), products(), products())
def test_multiply_products_associative(p1: Product, p2: Product, p3: Product):
    result_12 = multiply_products(p1, p2)
    result_23 = multiply_products(p2, p3)
    assert multiply_products(result_12, p3) == multiply_products(p1, result_23)
```
