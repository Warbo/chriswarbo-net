---
title: "Ivory: Manifest Decimation"
---

> If you can count to two, you can count to anything!
— <cite>Constable Cuddy, *Men At Arms* by Terry Pratchett</cite>

[Dyadic numbers](https://en.wikipedia.org/wiki/Dyadic_rational) are
[binary](https://en.wikipedia.org/wiki/Binary_number),
[floating point](https://en.wikipedia.org/wiki/Floating-point_arithmetic) and
[arbitrary-precision](https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic).
They are numbers of the form `(/ a (^ 2 n))`{.scheme} for any `integer` `a` and
`natural` `n`. Note that when `n` is `zero`, this simplifies to the `integer`
`a`:

```
(= (/ a (^ 2 0))
   (/ a 1)
   a)
```

Larger values of `n` give halves, quarters, eights, etc. `dyadic` is a
[ring](https://en.wikipedia.org/wiki/Ring_(mathematics)) which is closed under
`plus`, `times`, `max`, `min`, etc. Since `dyadic` contains every `integer`, but
only those fractions whose denominator is a power of `2`{.scheme}, it is not
closed under division: for example, `1` and `3` are `dyadic`, but
`(/ 1 3)`{.scheme} is not (since the denominator `3` is not a power of `2`).

Ivory includes `dyadic` since it is particularly simple, similar to the popular
IEEE754 format, and is commonly used in efficient numerical algorithms.

## Beyond binary ##

> | ALL YOUR BASE ARE BELONG TO US.
— <cite>CATS, *Zero Wing*</cite>

We can generalise `dyadic` by considering other bases besides `2`. Note that
making the base *variable*, like `(/ a (^ b n))`, allows any `rational` to be
represented; although raising to a power is redundant in that case, since
`(/ a b)` would suffice. However, there is more structure to expose before we
get that far!

Each `natural` gives rise to a different floating-point number system: besides
base `2`, other reasonable choices for Ivory include bases `3`, `6`, `8`, `10`,
`12`, `16`, `60` and `360`. Such number systems can, by definition, represent
fractions whose denominator is a power of the base; like powers of `2` for
`dyadic`. This also extends to powers of that base's
[*factors*](https://en.wikipedia.org/wiki/Integer_factorization): for example,
`3` is a factor of `60`, so base `60` can represent a fraction like
`(/ 1 (^ 3 7)`{.scheme}. Finally, *products* of such powers can also be
represented, e.g. both `3` and `4` are factors of `12`, so base `12` can
represent `(/ 1 (× (^ 3 3) (^ 4 5)))`. Note that `2` is also a factor of `12`,
so that last example could instead be expressed as `(/ 1 (× (^ 3 3) (^ 2 10)))`.

<figure>

```
┌─────────┬─────────┐
│ dozenal │ decimal │
├─ ─ ─ ─ ─┴─ ─ ─ ─ ─┤
│    sexagesimal    │
└───────────────────┘
```

</figure>

We require every level in Ivory to be a superset of those above, so any
floating-point level below `dyadic` must use a base with a factor of `2`, AKA an
an even number. This rules out base `3`, base `15`, etc.

The question is which *other* factors to include? The most natural is a factor
of `3`, giving us base `6`; or the equivalent yet more popular base `12`.
However, either of those would prevent us including a level for base `10`, which
is the most widespread number system in the world! We reconcile these
alternatives by choosing neither: instead, the `sexagesimal` level provides base
`60` fractions; with mezzanine siblings for `dozenal` (base `12`) and `decimal`
(base `10`).

Notice that this arrangement is completely unambiguous:

 - Fractions whose denominator is a power of `2`, like `(/ 1 2)`, `(/ 13 64)`,
   `(/ -9 16)`, etc. live in `dyadic`. Hence they are also `decimal`, `dozenal`
   and `sexagesimal` by inclusion.
 - Fractions whose denominator involves powers of `2` and `3`, like `(/ 1 3)`,
   `(/ -5 6)`, etc. live in `dozenal` and are included in `sexagesimal`. They
   are not `dyadic` or `decimal`.
 - Fractions whose denominator involves powers of `2` and `5`, like `(/ 1 5)`,
   `(/ -3 20)`, etc. live in `decimal` and are included in `sexagesimal`. They
   are not `dyadic` or `dozenal`.
 - Fractions whose denominator involves powers of `2`, `3` and `5`, like
   `(/ -2 45)`, live in `sexagesimal`. The yare not `dyadic`, `decimal` or
   `dozenal`.

After these, the next factor to consider would be `7`, but such number systems
do not see much use. Instead, we proceed on to the full `rational` level!
