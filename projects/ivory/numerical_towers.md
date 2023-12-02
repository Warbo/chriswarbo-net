---
title: "Ivory: Numerical Towers"
---

> The witchery that is cast in these towers
> It transcends the limits of my mind
> Ivory Towers in the sky
> A dreadful and enchanting sight
— <cite>Ivory Towers, Acid Mammoth</cite>

Numerical Towers are a particular way to design and organise a programming
language's numeric values and operations, notably used by the Lisp, Scheme &
Smalltalk families. We will characterised the approach as follows:

 - Multiple types of number are provided, e.g. `natural`, `integer`, `rational`,
   etc.
 - These types are constants, e.g. no parameterising like `(integer-modulo 7)`
   or `(matrix 3 3)`
 - Different types of number can use different representations, e.g. `integer`
   may use a sign to distinguish positives from negatives; whilst `rational` may
   store a separate numerator and denominator.
 - These types are related as strict subsets/supersets of each other, e.g. every
   `natural` is also an `integer`, and a `rational`, etc.
 - This subset relationship forms a "tower" (a total ordering between the types)
 - Each number is represented uniquely by its highest level in the tower. For
   example, `-12` is an `integer`, so there is no separate `rational` like
   `-12/1`. Likewise, there is no dedicated value for `9/12`, since that
   number's unique represention is `3/4`.
 - Numbers are equal precisely when their unique representations are identical.
 - Every number can be automatically "normalised" to its unique representation.
 - Numbers only store their intrinsic information, not where they're from or how
   they're supposed to be used.
 - Each arithmetic operation has a single definition, which handles any type of
   number (that it's defined for; e.g. we can't divide by a `zero`). There are
   no separate functions like `integer-+`, `rational-+`, etc.

For contrast, here are some features of *other* designs for numerical systems,
which are not numerical towers:

**Operator-based** approaches, also called modular programming or
programming-to-interfaces, focuses on the operations provided by each numerical
type; ignoring their underlying representations. For example, instead of
`natural`, `integer`, `rational`, etc. we may have `additive` (has a defined
addition operation), `invertable` (has a division operation), `ring` (has
multiplication, division and distributivity), `monoid` (has an associative
operation and identity element), etc. This approach can be seen in Haskell's
Typeclassopedia, and is also well-suited to ML signatures, Scala's type class
pattern, and (more awkwardly) to Java interfaces.

**Computer Algebra Systems**, like Maxima and SageMath, allow arbitrary
expressions to be represented symbolically, and rewritten via user-defined
rules. However, the price for this generality is that expressions do not have a
unique normal form, or any general algorithm for deciding their
equality. Computer Algebra is therefore reserved for interactive applications,
rather than used as a general purpose programming language.

**Parameterised types** allow more fine-grained control over what values and
operations are acceptable. For example, in Haskell the types `Sum Int`, `Product
Int`, `Sum Float` and `Product Float` are all different: "appending" values of
type `Sum t` will add them, and the "empty" value is the representation of zero
in type `t`; whilst values of type `Product t` will be multiplied, and the
"empty" value is the representation of one in type `t`. Dependently-typed
languages can go even further, with types like `Fin n` for numbers less than
`n`.

Finally, many programming languages don't provide any structured relationships
between their numerical types. For example, Python has types `int`, `float` and
`complex`, but they are not related (e.g. via Python's "subclass" mechanism).
Their values are disjoint, rather than nested, requiring multiple ways to
represent the same number (e.g. `2`, `2.0` and `2+0j`), with explicit coercion
required to turn one of those representations into another. Other languages,
like OCaml, even require separate operations for different numeric types, e.g.
providing `+` to add integers and `+.` to add floats.

## Tradeoffs ###

Organising numerical types into a linear "tower" provides more generality and
flexibility than unstructured systems. It is also simpler and less abstract than
operator-based or parameterised approaches, which are better suited to
statically checked languages. Numerical towers are a good fit for dynamic
languages.

This simplicity comes at the cost of having less precise types for our functions
to choose from. For example, in a numerical tower with `integer` as a subset of
`rational`, then any function accepting `rational` inputs *must* handle negative
values and zero values; we cannot restrict ourselves to e.g. "non-zero positive
rationals", since such a level would need to occur above `rational`, but cannot
be either above or below `zero` or `integer` (since it doesn't wholly contain,
and isn't wholly contained by, either). In such cases we may have to rely on
assertions, and convince ourselves that our usage is correct on a case-by-case
basis 🤞

### Mezzanines ###

<figure>

```
┌──────┬────────────┬────────────┐
│ dual │  complex   │ hyperbolic │
│      ├ ─ ─ ─ ─ ─ ─┤            │
│      │ quaternion │            │
├─ ─ ─ ┴ ─ ─ ─ ─ ─ ─┤            │
│  dual-quaternion  │            │
├─ ─ ─ ─ ─ ─ ─ ─ ─ ─┴─ ─ ─ ─ ─ ─ ┤
│           geometric            │
└────────────────────────────────┘
```

```
┌────────────┬─────────────┐
│ univariate │ monomial    │
│            ├ ─ ─ ─ ─ ─ ─ ┤
│            │ homogeneous │
├─ ─ ─ ─ ─ ─ ┴ ─ ─ ─ ─ ─ ─ ┤
│        polynomial        │
└──────────────────────────┘
```

 <figcaption>

Mezzanine structure inside the `geometric` and `polynomial` levels of the
Ivory tower. "Siblings" appear side-by-side, indicating that neither contains
the other.

 </figcaption>
</figure>

Whilst a case can be made for including this or that level, and perhaps having
some "sibling" levels where neither contains the other, there are ultimately too
many partially-overlapping cases to adequately express in a simple "tower"
structure. Since the whole point of a numerical tower is to simplify for common
cases, such relationships are perhaps better suited to a language/library based
on operators or parameterised types.

That said, Ivory does define additional structure *within* some levels, which we
call "mezzanines". Mezzanines cannot cross between multiple levels, so they do
not alter the linear structure of the tower's levels. The main purpose of
mezzanines is to support siblings, which allows many more useful types to be
defined that would otherwise conflict; with the overall tower being agnostic
about such internal distinctions.

For example, the `univariate-monomial` level contains expressions of the form
`nvᵃ`, where `n` is a `number`, `a` is a `natural` and `v` is an
[indeterminate](https://en.wikipedia.org/wiki/Indeterminate_%28%29). This is a
superset of `number`, since `(= v⁰ 1)`, so `(= nv⁰ n)`. There are two useful
levels we could put underneath, which generalise such expressions in different
ways:

 - `univariate` `polynomials` include *sums* as well as products, and have the
   form `av⁰+bv¹+cv²+…` for arbitrary `number` values `a`, `b`, `c`, …
 - Multivariate `monomials` allow arbitrarily-many indeterminates, with the form
   `nv₀ᵃv₁ᵇv₂ᶜ…` for arbitrary `number` `n` and `natural` values `a`, `b`, `c`,
   … (for consistency, `v₀` is a synonym for the `univariate` indeterminate `v`)

These both generalise to the `polynomial` level (sums and products with
arbitrarily-many indeterminates); yet making either of these a level between
`univariate-monomial` and `polynomial` would prevent the other from being
defined at all. Ivory avoids this dichotomy by choosing *neither*: instead going
straight to `polynomial`, and defining these finer-grained types as siblings
inside it.

In contrast, we *cannot* use mezzanines to define, say, a subset of `polynomial`
which only uses `rational` numbers, since no subdivisions *inside* the
`polynomial` level can change the fact that it's below (and hence wholly
contains) `geometric`, `algebraic`, etc.

### Operations ###

Numerical towers primarily focus on the representation of numbers, with their
operations being a secondary concern. A common pattern is for operations to
"lower" their inputs down the tower, to a level where that operation is defined.
For example, to subtract `natural` numbers we consider them as `integer`
numbers, since that can represent all of the possible results (which may be
negative). Special cases will be "lifted" back up by normalisation, e.g. if the
first input happens to be as large as the second, then the resulting `integer`
will be its `natural` subset; if the inputs happen to be equal, that `natural`
will also be in its `zero` subset; and so on.

Although a good rule of thumb, unfortunately this "lowering" behaviour doesn't
always hold; e.g. comparison functions like `≤` are not defined below the
`algebraic` level.
