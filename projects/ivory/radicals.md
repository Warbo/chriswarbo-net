---
title: "Ivory: Radicals"
packages: ['racketWithRackCheck']
---

> To be radical is to grasp things by the root
— <cite>Karl Marx, *Critique of Hegel's Philosophy of Right*</cite>

TODO:

 - `(expt x y)` is tricky:
   - When `y` is `natural` we get the same type as `x` (assuming it's closed
     under multiplication, which all of our types are)
   - When `y` is `integer` we get `rational` numbers or below
   - When `y` is `rational` we get `radical` and below
   - Probably don't want anything more general than `rational`, since their
     semantics involve logarithms, which I don't know a normal form for
   - Unclear what's the most general `x` we can support, when `y` is `rational`:
     - I think `radical` is OK; roots of products seem fine. Roots of sums
       don't easily rewrite (not sure of a general normal form for those...)
     - Roots become more, ahem, *complex* when dealing with `complex`, and other
       hypercomplex numbers; since sums can cancel-out when squared.
     - Seems reasonable to return `complex` when rooting a negative
     - If there's a symbolic normal form for other `geometric` numbers it would
       be nice to use that
     - Fractional powers of a `polynomial` should be representable using
       `algebraic-expression`, I think?
     - Note: don't confuse fractional powers of a `polynomial` with "roots"
       (substitutions for the indeterminate which evaluate to `0`)
 - Normalising `radical` requires factorising. That's slow, but also useful for
   cancelling common factors in a `rational`.
 - How to read and write powers of sums? Would like to avoid parentheses...
   - Don't!
 - Maybe we can construct roots based on an "index", e.g. we have (i n) to
   refer to the nth symbol that squares to -1; how about (√ 5 3) for the cube
   root of 5? Putting the "base" as first argument lets us default the second to
   2, so (√ 5) is short for (√ 5 2).
   - Note that (√ n 1) reduces to n
   - Composite numbers reduce to products, e.g. (√ 18) = (× (√ 2) (√ 3) (√ 3))
   - Products of roots reduce as appropriate, e.g. (× (√ 3) (√ 3)) = 3
 - The nice thing about this representation, is that we're not having to deal
   with all algebraic numbers; we're just simplifying the symbols that appear in
   a given expression.

NOTE: The set of algebraic numbers is larger than the set of radicals. Algebraic
numbers are roots of arbitrary univariate polynomials, but quintics and above
allow roots that can't be expressed using radicals!
