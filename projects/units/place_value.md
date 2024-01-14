---
title: Generalising place-value numbers to polynomials
packages: [ 'mathml' ]
---

"Place-value" numbers, also known as Hindu-Arabic numbers, are the 'normal' way
we write down numbers, e.g. `123`{.unwrap pipe="num | math"} to represent "a
hundred and twenty three" (AKA 'CXXIII' in roman numerals).

Place-value notation is usually taught very early on, since it's such a
fundamental part of how we do mathematics. That's good, but it also means we're
not very mathematically sophisticated when learning it: we might just rote-learn
some simple rules (and hopefully gain some intuition over time); once we *are*
more sophisticated, e.g. at high school, we may be so used to place-value
numbers that we never think to re-visit those rules, and the underlying theory.

## Simple rules ##

We may have learned place-value notation via rules like the following:

 * The right-most digit counts how many "ones" (or "units") we have
 * The left neighbour of a digit counts ten times more

Hence the digits, from right-to-left, count "ones", "tens", "hundreds",
"thousands", etc.

TODO: LINK TO LOGARITHMIC NAMING

```{pipe="sh > sum.mml"}
{
  {
    num '3'
    num '20'
    num '100'
  } | add
  num '123'
} | mapply 'eq'
```

Our example of `123`{.unwrap pipe="num | math"} contains
`3`{.unwrap pipe="num | math"} ones, `2`{.unwrap pipe="num | math"} tens and
`1`{.unwrap pipe="num | math"} hundred. The overall number is the sum of these
parts, so `cat sum.mml`{.unwrap pipe="sh | math"}.

Note that the 'ten times more' rule ensures we get zeros on the right, which
makes the final sum pretty easy.

## More sophisticated rules ##

Once we've spent a few years becoming comfortable with arithmetic, we can
re-visit the rules of place-value notation. In particular, we can make the above
rules more precise as *powers of a base*.

The "base" is the multiplier between adjacent digits; decimal numbers are "base
ten", but other bases are possible (and some are actually preferable!
TODO: LINK). Note that I'll be using the word "digit" regardless of base (rather
than "bit", "octet", "dozit", etc.).

The next key insight is that multiplying the base over and over (for each extra
digit going left) is the same as the *power* operation in arithmetic:

```{pipe="sh"}
{
  { num '10'; num '0'; } | mapply 'power'
  num '1'
} | mapply 'eq' > 0.mml

{
  { num '10'; num '1'; } | mapply 'power'
  { num '10'; num '1'; } | mult
  num '10'
} | mapply 'eq' > 1.mml

{
  { num '10'; num '2'; } | mapply 'power'
  { num '10'; num '10'; num '1'; } | mult
  num '100'
} | mapply 'eq' > 2.mml
```

`cat 0.mml`{.unwrap pipe="sh | math"}

`cat 1.mml`{.unwrap pipe="sh | math"}

`cat 2.mml`{.unwrap pipe="sh | math"}

For the above example:

```{.unwrap pipe="sh | math block"}
{
  num '123'
  {
    { num '1'; { num '10'; num '2'; } | mapply 'power'; } | mult
    { num '2'; { num '10'; num '1'; } | mapply 'power'; } | mult
    { num '3'; { num '10'; num '0'; } | mapply 'power'; } | mult
  } | add
  {
    { num '1'; num '100'; } | mult
    { num '2'; num  '10'; } | mult
    { num '3'; num   '1'; } | mult
  } | add
} | mapply 'eq'
```

We will keep things general by referring to our base using a variable
`b`{.unwrap pipe="var | math"}.

* Columns raise the base to increasing powers
** We tend to use base 10
*** Link to units post section on bases
** Power of 1 is just the number as-is
** Power of 0 is just the number 1 (except for 0, which doesn't work as a base anyway)
** Decimals raise the base to decreasing powers

* We can extend columns to the left, e.g. '123' is normally interpreted as
  '1 * 10^2 + 2 * 10^1 + 3 * 10^0', but can also be interpreted as
  '1 * 10^2 + 23 & 10^0' or '123 * 10^0'

* We can extend this interpretation to negative numbers too
** Link to negatives with bar notation
