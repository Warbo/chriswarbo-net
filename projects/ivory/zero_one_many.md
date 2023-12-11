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

<figure>

```
┌─────────┬─────────────────┐
│ boolean │   even-square   │
├─ ─ ─ ─ ─┴─ ─ ─┬─ ─ ─ ─ ─ ─┤
│     square    │ quadruple │
│               ├─ ─ ─ ─ ─ ─┤
│               │   even    │
├─ ─ ─ ─ ─ ─ ─ ─┴─ ─ ─ ─ ─ ─┤
│          natural          │
└───────────────────────────┘
```

</figure>

TODO

 - Describe zero, boolean, etc.
 - Closed under addition, multiplication, max, min, gcd, lcm, etc.
 - Defining boolean operations is easy, since there are so few; but need to be
   careful how they generalise to other numbers. For example, xor is often seen
   as addition mod 2, but that's hard to generalise to rationals, etc. Cleaner
   to use ≠
 - Describe thought process regarding `prime`:
   - Would need `0`, which is weird; but maybe a reasonable fudge (e.g.
     "primitive" or "primal" or something)
   - If we're going to include `0` then we should definitely include `1`, which
     puts it below `boolean`
   - Would need `2`, but can't be above/below `even`
   - Could maybe introduce 2 in a set {0, 2}, but seems contrived
   - Can't introduce 2 with {0, 1, 2} since it would need to appear above `even`
     but that doesn't include `1`
 - Describe through process regarding powers of two:
   - Would need `0`, which is awkward but still closed under many operations
   - Would need to generalise `boolean`, since 2⁰ = 1
   - Again, can't introduce 2 this way, since 1 isn't in `even`
   - Powers of 2 *other than* 1 and 2 are `quadruples`
   - Would conflict with `even-square`, since 16, 64, etc. are both

## Code ##

```{.unwrap pipe="./dump zero-one-many.rkt"}
```
