---
title: "Ivory: Zero"
packages: ['racketWithRackCheck']
---

```{pipe="sh"}
bash $setup zero-one-many.rkt
for DEPENDENCY in "$numbers_in_scheme"
do
  ./extract "$DEPENDENCY"
done
```

```{pipe="./hide"}
#lang racket
(module+ test (require rackunit rackcheck-lib))
```

`zero` is the highest level that actually contains values. In fact it contains a
*single* value `(+)`:

```unwrap
(define zero? (curry equal? '(+))
```

This value (a list containing the symbol `+`) represents *the empty sum*, i.e.
what we get when adding nothing together. Since it's a sum, its behaviour can be
derived from the [sum and product laws](sums_and_products.html#laws-of-algebra).

## Addition ##

Addition is **associative**: nested sums like `(+ a (+ b c) d)`{.scheme} are
equal to a single sum containing all of those elements, i.e.
`(+ a b c d)`{.scheme}.

Since the empty sum `(+)`{.scheme} contains no elements, its presence in a sum
makes no difference to the result, e.g. `(= (+ x (+) y) (+ x y))`{.scheme}. In
other words, it is the **identity element** for addition.

If *all* of the elements of a sum are empty sums, the result is equal to the
empty sum; for example `(+ (+) (+) (+))`{.scheme} is equal (via associativity)
to `(+)`{.scheme}. Hence summing any `zero`{.scheme} values results in another
`zero`{.scheme} value, making `zero` *closed under addition*.

## Multiplication ##

We can likewise derive the behaviour of `(+)`{.scheme} in a product. This time,
we use **distributivity**: a product containing a sum, like
`(× a b (+ c d) e)`{.scheme}, equals a sum of products; in this case
`(+ (× a b c e) (× a b d e))`{.scheme}. Importantly, that original sum has the
same number of elements as the result; if the original was empty, so is the
result! For example `(× a b (+) d)`{.scheme} equals `(+)`{.scheme}, and likewise
for any product containing the empty sum `(+)`{.scheme}. This makes
`(+)`{.scheme} the **absorbing element** for multiplication; and `zero`{.scheme}
is *closed under multiplication*.

## Negation ##

**Distributivity** also applies to negation, so `(- (+))`{.scheme}

## Other operations ##

`zero`{.scheme} is closed under negation, since

`zero`{.scheme} is trivially closed under `min`{.scheme} and `max`{.scheme},
since those return one of their given arguments.

We can extend the `gcd`{.scheme} ([greatest common
divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor)) and
`lcm`{.scheme} ([least common
multiple](https://en.wikipedia.org/wiki/Least_common_multiple)) functions to
operate on values from `zero`{.scheme}; in which case those are also closed.



`zero` is hence a
[unit type](/blog/2020-02-09-bottom.html), and a
[terminal object](https://en.wikipedia.org/wiki/Initial_and_terminal_objects).
It is dual to [empty type `void`](void.html).

This level seems rather trivial, but nevertheless it is a valid number system,
which is closed under common operations like `+`, `-`, `×`, `max`, `min`, `gcd`
and `lcm`.
