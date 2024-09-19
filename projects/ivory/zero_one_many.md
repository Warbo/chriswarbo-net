---
title: "Ivory: Zero, One, Many"
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

## `natural` ##

> | Wheels within wheels
> | In a spiral array
> | A pattern so grand
> | And complex
- <cite>Rush, *Natural Science*</cite>

The next level from `zero` is `natural`, which contains all of the "counting"
numbers: `0`, `1`, `2`, `3`, etc. There is rich structure in the `natural`
numbers, but our requirement for levels to follow a subset relation constrains
how much we're able to represent.

Ivory exposes the following subsets as [mezannines]() *within* the overall
`natural` level:

<figure>

```
┌─────────┬───────────────────┐
│ boolean │    even-square    │
├─ ─ ─ ─ ─┴─ ─ ─┬─ ─ ─ ─ ─ ─ ─┤
│     square    │ doubly-even │
│               ├─ ─ ─ ─ ─ ─ ─┤
│               │     even    │
├─ ─ ─ ─ ─ ─ ─ ─┴─ ─ ─ ─ ─ ─ ─┤
│            natural          │
└─────────────────────────────┘
```

</figure>

### `boolean` ###

This is home to the number `1`, which (along with `0` inherited from `zero`)
forms a simple but important numerical system. The `boolean` numbers are closed
under `×`, `max`, `min`, `gcd`, `lcm` and `/` (excluding division by `0`).

There are many operations which make sense for `boolean` but do not generalise
well to other levels. For example, the [`xor` operation]() is often seen as a
form of addition ([modulo 2]()), but that perspective doesn't make much sense
for supersets like `rational`. If we instead consider `xor` to be a specialised
form of `≠` then it generalises naturally.

### `even-square` ###

This subset contains
[even square numbers](https://en.wikipedia.org/wiki/Square_number#Odd_and_even_square_numbers),
of the form `(^ (× 2 n) 2)` for any `natural` `n`: this reduces to `0` in the
case that `(= n 0)`. The utility of this mezzanine is limited on its own, but
its presence allows the `square` and `even` subsets to coexist.

### `square` ###

This extends the contents of `even-square` to all
[square numbers](https://en.wikipedia.org/wiki/Square_number) of the form
`(^ n 2)`; with `(= (^ 1 2) 1)` inherited from `boolean`.

### `doubly-even` ###

The [doubly-even numbers](https://en.wikipedia.org/wiki/Singly_and_doubly_even)
numbers have the form `(× 4 n)` for some `natural` `n`, i.e. multiples of `4`,
or `natural` numbers with `4` as a factor. This is a strict extension of
`even-square`, since:

```
(= (^ (× 2 n) 2)
   (× (× 2 n) (× 2 n))
   (× 2 2 n n)
   (× 4 n n)
   (× 4 (^ n 2)))
```

This set is closed under `+`, `×`, `^`, `max`, `min`, `gcd`,
`lcm`, etc. and is useful in certain number-theoretic contexts.

### `even` ###

The [even numbers]() are multiples of `2`, of the form `(× 2 n)` for some
`natural` `n`. When `n` is itself `even`, the result is `doubly-even`; when `n`
is `doubly-even` the result is triply-even, etc. This set
is closed under `+`, `×`, `^`, `max`, `min`, `gcd`, `lcm`, etc.

### Leftovers ###

The bottom of the `natural` level fills in any remaining gaps: i.e. odd numbers
which are not `square`. Many of these remaining numbers will be [odd primes](),
which would be useful to represent, but doesn't fit as a mezzanine since it's
not a superset of `zero`.

 - Describe through process regarding powers of two:
   - Would need `0`, which is awkward but still closed under many operations
   - Would need to generalise `boolean`, since 2⁰ = 1
   - Again, can't introduce 2 this way, since 1 isn't in `even`
   - Powers of 2 *other than* 1 and 2 are `quadruples`
   - Would conflict with `even-square`, since 16, 64, etc. are both

## Code ##

```{.unwrap pipe="./dump zero-one-many.rkt"}
```
