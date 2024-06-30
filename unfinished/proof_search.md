---
title: Proof search
---

Norman Wildberger posted a video recently, demonstrating the problems which can
arise when treating never-ending processes as if they have a "result". This is
adjacent to a few areas I like to explore, so it inspired a little elaboration,
if you'll indulge me!

## Summing a never-ending sequence ##

Wildberger uses the classic example of summing the numbers ½ⁿ, with n counting
up from 1, so we'll start there. This sum can be represented quite succinctly in
Haskell as follows:

```haskell
total = sum (map ((1 / 2) ^) (enumFrom 1))
```

Here, `enumFrom 1`{.haskell} generates a never-ending list counting up from `1`
(where `enumFrom n = n : enumFrom (1 + n)`{.haskell}). We transform the elements
of that list using `map`{.haskell}, with the transformation given by
`((1 / 2) ^)`{.haskell} which raises ½ to the given power.

Of course, this program will run forever (at least, until we turn it off); but
that's not *necessarily* a bad thing. For example, on a life-support machine we
*want* the software to keep going; it would be terrible if such programs decided
to just "finish"! It's better to characterise *how* this program runs forever.

## Co-recursion, co-induction and co-termination ##

The definition of `map` is a familiar, recursive walk along a given list:

```haskell
map f (x : y) = f x : map f y
map f []      = []
```

In Haskell, the list constructors are `[]`{.haskell}
(pronounced "nil"), which represents the empty list; and `x : y`{.haskell}
(pronounced "`x` [cons](https://en.wikipedia.org/wiki/Cons#Lists) `y`"),
representing a list whose first ("head") element is `x`, followed by the rest of
the list `y` (its "tail", which may be `[]`). For example
`1 : 2 : 3 : []`{.haskell} is a list containing elements `1`, `2` and `3`, in
that order.
; which
we can characterise by how it breaks-down each "constructor" (AKA introduction
form) of the input list.

The behaviour of `map` can be specified by "pattern-matching" its argument list
against these constructors:


In contrast, we can specify `enumFrom`{.haskell} like this:

```haskell
head (enumFrom n) = n
tail (enumFrom n) = enumFrom (n + 1)
```

has no such "base case": each recursive call
makes another; incrementing the counter as it goes. This is not "well-founded",
and would be frowned upon in many introductory programming courses.

In fact, we can understand `enumFrom` as an example of
[co-recursion](https://en.wikipedia.org/wiki/Corecursion). In mathematics, the
prefix "co-" usually involves something being done "the other way around". Co-recursive
definitions (like `enumFrom`) are characterised "the other way around": by how
they build up each "destructor" (AKA elimination form) of their output. In
particular,




goes by how they ase this case we  means putting usually One way to see the difference is that recursive definitions depend  is to avoid thinking input is constructed Haskell is
well-suited for this, since it calculates values on-demand (AKA "lazily"); in
contrast to most languages, which calculate values up-front (AKA
"eagerly"). For more detail I recommend [Why Functional Programming
Matters](https://www.researchgate.net/publication/2452204_Why_Functional_Programming_Matters).



The definition of `total1`{.haskell} is "polymorphic", since the implementations
of `+`{.haskell}, `/`{.haskell}, `^`{.haskell}, `1`{.haskell} and `2`{.haskell}
depend on which type we use; e.g. using `total1 :: Float`{.haskell} will use a
floating-point representation, whilst `total1 :: Data.Ratio.Rational`{.haskell}
will use a ratio of bignums. However, regardless of which type we choose, this
calculation will always diverge since `sum`{.haskell} is defined as a "left
fold", like:

```haskell
sum = sumFrom 0
  where sumFrom n (x : y) = sumFrom (n + x) y
        sumFrom n []      = n
```

(The actual definition is more complicated, since it's polymorphic in the type
of structure being folded, but this is accurate for the list structure we're
using!)

We can ignore the base case for `[]`{.haskell} (the empty list), since our
fractions never end. The calculation diverges since the recursive call to
`sumFrom`{.haskell} is made *directly*: the `+`{.haskell} function (which we can
choose, thanks to polymorphism) only appears in an argument, and hence has no
control over the recursion.

We can fix this by summing with a "right fold":

```haskell
total2 = sumR (map ((1 / 2) ^) (enumFrom 1))
  where sumR (x : y) = x + sumR y
        sumR []      = 0
```

Again, we can ignore the base case for `[]`{.haskell}. Notice that
`sumR`{.haskell} calls the `+`{.haskell} function directly, and this time the
recursive call `sumR y`{.haskell} occurs as an argument. The implementation of
`+`{.haskell} depends on the type we choose for our numbers; with care, we can
prevent it from evaluating the recursive call (thanks to laziness), and hence
avoid diverging.

### Productivity from laziness ###

We can't do an infinite amount of work in finite time, but we can *put off* as
much works as possible, thanks to laziness.

That is another never-ending
process, but it's not as useful as `enumFrom`{.haskell}, `map`{.haskell} or
`fractions`{.haskell}, since it's not "productive" (it "diverges", whilst the
others "co-terminate"). The latter processes will carry on spitting out data as
long as we keep asking for more; yet `sum fractions` will never produce *any*
data! That's because we're asking it for a single `Rational`{.haskell} number
(represented as a pair of `Integer`{.haskell} numerator and denominator), but it
can't return any particular integers until it's "finished" adding up (which will
never happen).

We can avoid this by replacing the numerator-over-denominator representation
with an alternative, that's more suited to being generated piece by piece. In
this case, the negative powers of two act like "binary scientific notation",
corresponding with the "decimal places" of a binary number; e.g. the 5 terms
shown above correspond to the binary numbers
`[0.1,0.01,0.001,0.0001,0.00001]`{.haskell}. We can encode such numbers as a
list of their bits (we'll omit the leading `0.`{.haskell}):

```haskell
data Bit = Zero | One
type Binary = [Bit]

binaryFractions = map halfToPower (enumFrom 1)
  where halfToPower 1 = One  : allZeros
        halfToPower n = Zero : halfToPower (n - 1)

        allZeros = Zero : allZeros
```

We want to add these `Binary`{.haskell} numbers in a way that's productive:
generating each `Bit`{.haskell} of the sum one by one, with a finite amount of
steps before the next `Bit`{.haskell} is produced. That's only possible if there
are no carries: for example the first fractional `Bit`{.haskell} of
`0.0111… + 0.0000…` could be either `Zero`{.haskell} or `One`{.haskell}, since a
carry from further down the sequence may bubble-up through that string of ones.
In such cases we can't return a `Bit`{.haskell} until we find the end of those
consecutive strings, to see whether there's a carry or not; but they might go on
forever!

Thankfully our sum is guaranteed to have no carries, since each number
contributes `One`{.haskell} to a different place; making the addition
equivalent to the [bit-wise
OR](https://en.wikipedia.org/wiki/Bitwise_operation#OR). We can implement this
productively by having the calculation of each `Bit`{.haskell}
[short-circuit](https://en.wikipedia.org/wiki/Short-circuit_evaluation) when it
hits a `One`{.haskell}:

```haskell
addAllWithoutCarry xs = or (map head xs) : addAllWithoutCarry (map tail xs)
  where head (a : b) = a
        tail (a : b) = b
        or (One  : rest) = One
        or (Zero : rest) = or rest
```

This is only productive if each column short-circuits after a finite number of
elements. That is the case for `binaryFractions`{.haskell}, since each
`Bit`{.haskell} on the "diagonal" is `One`{.haskell}. Hence the entire sum
`addAllWithoutCarry binaryFractions`{.haskell} is equivalent to the never-ending
process `allOnes = One : allOnes`{.haskell}.
