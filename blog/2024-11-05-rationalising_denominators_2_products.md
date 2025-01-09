---
title: "Rationalising Denominators 2: Products"
---

Posts in this series:

[Powers](/blog/2024-11-03-rationalising_denominators_1_powers.html)

[Products](/blog/2024-11-05-rationalising_denominators_2_products.html)

[Sums](/blog/2024-11-08-rationalising_denominators_3_sums.html)

[Ratios](/blog/2024-11-13-rationalising_denominators_4_ratios.html)

---

**Note:** This page deals with a few different concepts, which I've tried to use
in a consistent way: "numbers" are the platonic entities we want to talk about;
mathematical notation is used to describe numbers; and Python code is used for
our specific, concrete implementation. I've annotated the mathematical notation
with its corresponding Python code, where relevant.

## Introduction ##

[Last time](/blog/2024-11-03-rationalising_denominators_1_powers.html) we used
Python's `int`{.python} and `Fraction`{.python} types to implement a new type
called `Power`{.python}, to represent
[nth roots](https://en.wikipedia.org/wiki/Nth_root).

We implemented a few normalisation steps, to avoid redundant values like
<abbr title="Power(1, Fraction(2, 1))">1²</abbr> (which is just
<abbr title="Power(1, Fraction(0, 1))">1</abbr>) and
<abbr title="Power(0, Fraction(3, 1))">0³</abbr> (which is just
<abbr title="Power(0, Fraction(1, 1))">0</abbr>).

We were still left with some redundancy though, like
<abbr title="Power(4, one)">4¹</abbr> and
<abbr title="Power(2, Fraction(2, 1))">2²</abbr>` both being the number 4; or,
conversely, <abbr title="Power(2, Fraction(1, 1))">2¹</abbr> and
<abbr title="Power(4, half)">√4</abbr> both being the number 2.

This time we'll use `Power`{.python} to implement another type, called
`Product`{.python}, which completely avoids these redundancies.

## Products ##

A product is a collection of numbers multiplied together. We can define a
`Product`{.python} type in Python as a container of `Power`{.python} values:

```python
def Product(*powers: Power) -> List[Power]:
    return list(powers)
```

This can represent all the same values as `Power`{.python} (by using a list with
a single value), but it lets us represent some values in a different way. For
example, `Power`{.python} can only represent some numbers by taking a large root
of a large base, like
<abbr title="Power(72, Fraction(1, 6))">⁶√72</abbr>; whilst `Product`{.python}
can break that down into
<abbr title="Product(Power(3, Fraction(1, 3)), Power(2, Fraction(1, 2)))">∛3×√2</abbr>.

Here are some useful constants for this type:

```python
product_zero = Product(power_zero)
product_one = Product(power_one)
```

### Improving our representation ###

Since we're dealing with [commutative]() multiplication (for the moment),
changing the order of arguments given to `Product`{.python} should not affect
its result; yet the order *does* matter if we represent them with a `list`.
Instead, we could use a [bag]() (AKA a multiset); which can be implemented as a
dictionary which counts the number of times a particular `Power`{.python}
appears:

```python
def Product(*powers: Power) -> List[Power]:
    counts = {}
    for power in powers:
        counts[power] = counts.get(power, 0) + 1
    return counts
```

However, we can exploit the semantics of `Power`{.python} to get a simpler
representation: multiplying two `Power`{.python} values with the same
`base`{.python} results in that `base`{.python} raised to the *sum* of their
`exp`{.python}onents. This means our dictionary can directly associate
`base`{.python}s with (sums of) `exp`{.python}onents:

```python
def Product(*powers: Power) -> List[Power]:
    result = {}
    for base, exp in powers:
        result[base] = result.get(base, zero) + exp
    return result
```

To turn such a `Product`{.python} back into a list of `Power`{.python} values,
we can call its `.items()`{.python} method.

## Multiplication ##

Now we've chosen the final structure of `Product`{.python}, we can write a
multiplication function for it. Since `Product`{.python} already represents a
multiplication of its contents, we can multiply `Product`{.python} values
together by collecting up all their elements into one big `Product`{.python}:

```python
def multiply_products(*ps: Product) -> Product:
    return Product(*sum([list(p.items()) for p in ps], []))
```

## Normalisation ##

There is plenty of redundancy in this simple definition, so we'll build up a
normalisation algorithm which consolidates such values to a single
representation.

### Absorbing Zero ###

All products which contain <abbr title="Power(0, Fraction(1, 1))">0</abbr> are
equivalent (since 0 is the
[absorbing value](https://en.wikipedia.org/wiki/Absorbing_element) for
multiplication). The simplest of these is the product which *only* contains
0, so we'll normalise the others to that:

```python
        if base == 0:
            # If any element is zero, the whole Product is zero
            return dict([power_zero])
```

### Discarding Ones ###

The number <abbr title="Power(1, Fraction(0, 1))">1 is the
[identity value](https://en.wikipedia.org/wiki/Identity_element) for
multiplication, so its presence in a product does not alter the result. Hence
the normalisation of a `Product`{.python} should remove any such
`Power`{.python}. Thankfully, we can rely on `Power`{.python} to normalise such
values to <abbr title="Power(1, Fraction(1, 0))">`power_one`{.python}<abbr>,
which we can check for in our loop:

```python
        if (base, exp) != power_one:
            result[base] = result.get(base, zero) + exp
```

Note that, if *every* `Power`{.python} given to `Product`{.python} is 1, then
the result will be the
[empty product](https://en.wikipedia.org/wiki/Empty_product); and this is the
normal form for the `Product`{.python} representing the number 1!

### Fixed Points ###

Performing multiple checks in sequence may cause us to miss redundancies, where
earlier checks could normalise the results of later ones. There are many ways we
could adjust our code to fix this, but one particularly simple approach is to
call ourselves recursively, until our result matches our input (i.e. until we
reach a [fixed point](https://en.wikipedia.org/wiki/Fixed_point_(mathematics))):

```python
    return result if len(powers) == len(result) and result == dict(powers) \
        else Product(*result.items())
```

### Factorising ###

Now we confront the main source of redundancy in both `Product`{.python} and
`Power`{.python}: that different sets of powers can multiply to the same result;
and indeed that raising different `base`{.python} values to appropriate
`exp`{.python}onents can also produce the same result.

We will avoid this redundancy by only allowing elements of a `Product`{.python}
to have `base`{.python} values which are
[prime](https://en.wikipedia.org/wiki/Prime_number), or the special value
`0`{.python} (we will actually revisit this in a later post, but that's for
another day!).

To implement this normalisation, we need some helper functions to
[factorise](https://en.wikipedia.org/wiki/Integer_factorization) `base`{.python}
values. There are many algorithms we could use, and the details aren't too
important for our purposes; you can expand the following box to see the
implementation I've used.

<details class="odd">
<summary>Implementation of `factorise`{.python}</summary>

Factorising can be slow, and was the bottleneck in my initial experiments. The
following function uses a few tricks to speed up calculations, compared to a
naive approach:

 - The `lru_cache`{.python} decorator gives the function a
   [memo table](https://en.wikipedia.org/wiki/Memoization), so results which
   have previously been calculated can be quickly re-used. Note that we cannot
   use this decorator on a recursive function (like `Product`{.python} itself),
   since it results in weird inconsistencies!
 - During startup there is a one-time cost to calculate the primes up to 1000,
   which are used to factor small numbers.
 - If we have to calculate the factors of some large number, we use a "wheel
   factorisation" algorithm, which is at least faster than brute-force search!

```python
from functools import lru_cache

def factorise() -> Callable[[Base], Product]:
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
        factors = {}

        # Handle 2, 3 and 5 separately for efficiency
        for p in (2, 3, 5):
            if n % p == 0:
                factors[p] = Fraction(0, 1)
                while n % p == 0:
                    factors[p] += 1
                    n //= p

        # Wheel increments for numbers coprime to 2, 3 and 5
        increments = [4, 2, 4, 2, 4, 6, 2, 6]
        i = 0
        f = 7

        while f * f <= n:
            if n % f == 0:
                factors[f] = Fraction(0, 1)
                while n % f == 0:
                    factors[f] += 1
                    n //= f
            f += increments[i]
            i = (i + 1) % 8

        if n > 1:
            factors[n] = Fraction(1, 1)

        return factors

    @lru_cache(maxsize=1024)
    def factorise(n: int) -> Product:
        if n == 0:
            return dict([power_zero])
        if n == 1:
            return dict([power_one])
        if n < 0:
            # Negatives have negative one as a factor
            factors = factorise(-n)
            factors[1] = factors.get(1, Fraction(0, 1)) + Fraction(1, 2)
            return factors
        if n > 1000:
            # Fallback method for large numbers
            return wheel_factorise(n)
        else:
            # Small numbers can use precomputed primes
            factors = {}
            for p in SMALL_PRIMES:
                if p * p > n:
                    if n > 1:
                        factors[n] = one
                    break

                if n % p == 0:
                    factors[p] = zero
                    while n % p == 0:
                        factors[p] += 1
                        n //= p
            return factors
    return factorise
factorise = factorise()
```

</details>

We use that `factorise`{.python} function to implement the following
`split_power`{.python} function, which adjusts factors of a `base`{.python} to
account for the given `exp`{.python}onent:

```python
def split_power(power: Power) -> List[Power]:
    base, exp = power
    return [Power(b, exp * e) for b, e in factorise(base).items()]
```

Then, rather than `Product`{.python} accumulating its given `Power`{.python}
arguments as-is, it can instead accumulate the results of `split_power`{.python}
called on those arguments:

```python
def Product(*powers: Power) -> dict[Base, Exponent]:
    result = {}
    for power in powers:
        for base, exp in split_power(Power(*power)):
            if base == 0:
                # If any element is zero, the whole Product is zero
                return dict([power_zero])
            b, e = Power(base, result.pop(base, zero) + exp)
            if (b, e) != power_one:
                result[b] = e
    return result if len(powers) == len(result) and result == dict(powers) \
        else Product(*result.items())
```

The redundancy in our earlier examples is now avoided: since 4 is not prime, any
`Power`{.python} with `base`{.python} 4 will be replaced by an equivalent
involving `base`{.python} 2:

 - <abbr title="Product(Power(4, Fraction(1, 1)))">4¹</abbr> will normalise to
   <abbr title="Product(Power(2, Fraction(2, 1)))">2²</abbr>.
 - <abbr title="Power(4, Fraction(1, 2))">√4</abbr> will normalise to
   <abbr title="Power(2, Fraction(1, 1))">2¹</abbr>.

Note that we don't perform this factorising in `Power`{.python}, since it may
require a `Product`{.python} of multiple elements. For example, normalising
<abbr title="Product(Power(18, Fraction(2, 1)))">18²</abbr> produces
<abbr title="Product(Power(2, Fraction(2, 1)), Power(3, Fraction(4, 1)))">2²×3⁴</abbr>.

<!--

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
-->

## Extra Helpers ##

We'll be needing the following helper functions, so might as well define them
here alongside `Product`{.python}:

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

We've extended our representation of numbers from `Power`{.python}, which
suffered from redundancy; to `Product`{.python}, which does not have any
redundancies. Normalising this `Product`{.python} representation has exposed
some structure that will be useful in future installments: namely the use of
*prime factors*<!-- and *roots of unity*-->.

In [the next post](/blog/2024-11-08-rationalising_denominators_3_sums.html)
we'll extend our representation again: to *sums* of products of powers!

## Appendixen ##

### Implementation ###

Here's the final implementation of `Product`{.python}, assuming that we have an
implementation of `Power`{.python} defined:

```python
def Product(*powers: Power) -> dict[Base, Exponent]:
    result = {}
    for power in powers:
        for base, exp in split_power(Power(*power)):
            if base == 0:
                # If any element is zero, the whole Product is zero
                return dict([power_zero])
            b, e = Power(base, result.pop(base, zero) + exp)
            if (b, e) != power_one:
                result[b] = e
    return result if len(powers) == len(result) and result == dict(powers) \
        else Product(*result.items())

def multiply_products(*ps: Product) -> Product:
    return Product(*sum([list(p.items()) for p in ps], []))

def eval_product_int(p: Product) -> int:
    return reduce(
        lambda x, y: x * eval_power_int(y),
        p.items(),
        1
    )

def evaluate_product(p: Product) -> float:
    return reduce(lambda acc, f: acc * evaluate_power(f), p.items(), 1.0)

def product_is_rational(p: Product) -> bool:
    return all(power_is_rational(x) for x in p.items())

def split_power(power: Power) -> List[Power]:
    base, exp = power
    return [Power(b, exp * e) for b, e in factorise(base).items()]

def factorise() -> Callable[[Base], Product]:
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
        factors = {}

        # Handle 2, 3 and 5 separately for efficiency
        for p in (2, 3, 5):
            if n % p == 0:
                factors[p] = Fraction(0, 1)
                while n % p == 0:
                    factors[p] += 1
                    n //= p

        # Wheel increments for numbers coprime to 2, 3 and 5
        increments = [4, 2, 4, 2, 4, 6, 2, 6]
        i = 0
        f = 7

        while f * f <= n:
            if n % f == 0:
                factors[f] = Fraction(0, 1)
                while n % f == 0:
                    factors[f] += 1
                    n //= f
            f += increments[i]
            i = (i + 1) % 8

        if n > 1:
            factors[n] = Fraction(1, 1)

        return factors

    @lru_cache(maxsize=1024)
    def factorise(n: int) -> Product:
        if n == 0:
            return dict([power_zero])
        if n == 1:
            return dict([power_one])
        if n < 0:
            # Negatives have negative one as a factor
            factors = factorise(-n)
            factors[1] = factors.get(1, Fraction(0, 1)) + Fraction(1, 2)
            return factors
        if n > 1000:
            # Fallback method for large numbers
            return wheel_factorise(n)
        else:
            # Small numbers can use precomputed primes
            factors = {}
            for p in SMALL_PRIMES:
                if p * p > n:
                    if n > 1:
                        factors[n] = one
                    break

                if n % p == 0:
                    factors[p] = zero
                    while n % p == 0:
                        factors[p] += 1
                        n //= p
            return factors
    return factorise
factorise = factorise()

@lru_cache(maxsize=1024)
def is_prime(n: int) -> bool:
    if n == 0:
        return False
    if n == 1:
        return False
    factors = factorise(n)
    return n in factors

product_zero = Product(power_zero)
product_one = Product(power_one)
```

### Property Tests ###

The following test suite uses [Hypothesis](https://hypothesis.readthedocs.io)
and [Pytest](https://pytest.org) to check various properties that should hold
for our `Product`{.python} type:

```python
import pytest
from hypothesis import assume, given, strategies as st, note

debug = lambda **xs: (note(repr(dict(**xs))), list(xs.values())[0])[-1]

# Test strategies
def primes():
    first_primes = list(filter(is_prime, range(2, 10)))
    def primes():
        return st.sampled_from(first_primes)
    return primes
primes = primes()

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

@st.composite
def powers(draw, rational=None):
    # Avoid base 0 when rational=False
    b = draw(st.integers(min_value=1 if rational == False else 0, max_value=3))
    e = draw(fractions(rational=rational))

    # Avoid 0^0, which is undefined
    b, e = Power(b, one if b == 0 and e == 0 else e)

    # Avoid fractional powers of 1 (for now...)
    if b == 1 and e > 0 and e < 1:
        b, e = power_one if rational else root_two

    return (b, e)

@st.composite
def products(draw, rational=None):
    raw = draw(st.lists(
        # Rational products need entirely rational powers (when in normal
        # form), but irrational products don't need *all* of their powers
        # to be irrational
        powers(rational=rational if rational else None),
        min_size=1 if rational == False else 0,
        max_size=3
    ))
    if rational == None:
        # No need to enforce anything, just return it
        return Product(*raw)

    if rational:
        result = Product(*raw)
        assert product_is_rational(result)
        return result

    # Being irrational is harder. We can improve our chances by skipping zeros
    # (as they'll absorb everything else, including any irrationalities)
    tail = [(base, exp) for base, exp in raw if base != 0]

    # If we just-so-happen to be irrational already, then we're done:
    attempt = Product(*tail)
    if not product_is_rational(attempt):
        return attempt

    # Otherwise, we need to insert another irrational, since the list could have
    # been emptied out when we discarded zeros. Thankfully, there's no chance
    # that this irrational will cancel-out with something else, since we already
    # know that the result so far is rational!
    head = draw(powers(rational))
    result = Product(head, *tail)

    if product_is_rational(result):
        # Shouldn't get here, bail out!
        details = repr(dict(
            rational=rational,
            raw=raw,
            head=head,
            tail=tail,
            attempt=attempt,
            result=result
        ))
        assert False, f"Error: {details}"
    return result

def whole_products():
    return products().map(lambda p: Product(*[
        Power(base, Fraction(exp.numerator)) for base, exp in p.items()
    ]))

# Tests

@given(st.data())
def test_strategy_primes(data):
    data.draw(primes())

@given(st.data())
def test_strategy_powers_can_force_rational(data):
    assert data.draw(powers(rational=True))[1].denominator == 1

@given(st.data())
def test_strategy_powers_can_force_irrational(data):
    assert data.draw(powers(rational=False))[1].denominator > 1

@given(st.data())
def test_strategy_products(data):
    data.draw(products())

@given(products(rational=True))
def test_strategy_products_can_force_rational(p):
    assert product_is_rational(p)

@settings(suppress_health_check=[HealthCheck.too_slow])
@given(products(rational=False))
def test_strategy_products_can_force_irrational(p):
    assert not product_is_rational(p)

@given(st.integers(min_value=2, max_value=1000))
def test_prime_factors_are_prime(n: int):
    prime_factors = debug(prime_factors=factorise(n))
    for base, _ in prime_factors.items():
        assert is_prime(base)

@given(st.integers(min_value=2, max_value=1000))
def test_prime_factors_multiply_to_input(n: int):
    prime_factors = debug(prime_factors=factorise(n))
    product = debug(product=eval_product_int(prime_factors))
    assert product == n

@given(products())
def test_normalise_product_idempotent(p: Product):
    assert p == Product(*p.items())

@given(st.lists(
    st.integers(min_value=0, max_value=1000),
    min_size=0,
    max_size=3
))
def test_normalise_product_composites(l: List[int]):
    normal = Product(*[Power(base, Fraction(1)) for base in l])
    for base, exp in normal.items():
        assert exp.denominator == 1, f"{normal} contains fractional power {exp}"

@settings(suppress_health_check=[
    HealthCheck.filter_too_much,
    HealthCheck.too_slow,
])
@given(products(), products())
def test_normalise_product_absorbs(pre: Product, suf: Product):
    assert product_zero == Product(*(
        list(pre.items()) + [power_zero] + list(suf.items())
    ))

@given(whole_products(), whole_products())
def test_multiply_products_agrees_with_evaluated_int(p1, p2):
    i1 = eval_product_int(p1)
    i2 = eval_product_int(p2)
    i3 = eval_product_int(multiply_products(p1, p2))
    assert i3 == i1 * i2

@given(st.lists(powers()))
def test_multiply_products_agrees_with_evaluated_float(powers):
    try:
        individual = debug(individuals=[evaluate_power(p) for p in powers])
        evaluated = debug(evaluated=evaluate_product(Product(*powers)))
    except Complex:
        assume(False)
    multiplied = debug(multiplied=reduce(lambda x, y: x * y, individual, 1.0))
    assert multiplied - evaluated < 10e-9

@given(products())
def test_multiply_products_identity(p: Product):
    assert multiply_products(p, product_one) == p, "1 is right identity"
    assert multiply_products(product_one, p) == p, "1 is left identity"

@settings(suppress_health_check=[HealthCheck.filter_too_much])
@given(products(), products())
def test_multiply_products_commutative(p1: Product, p2: Product):
    result_12 = multiply_products(p1, p2)
    result_21 = multiply_products(p2, p1)
    assert result_12 == result_21

@settings(suppress_health_check=[
    HealthCheck.filter_too_much,
    HealthCheck.too_slow
])
@given(products(), products(), products())
def test_multiply_products_associative(p1: Product, p2: Product, p3: Product):
    result_12 = multiply_products(p1, p2)
    result_23 = multiply_products(p2, p3)
    assert multiply_products(result_12, p3) == multiply_products(p1, result_23)
```

<!-- TODO: Run these tests ;) -->
