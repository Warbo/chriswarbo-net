---
title: "Ivory: Radicals"
packages: ['racketWithRackCheck']
---

> To be radical is to grasp things by the root
— <cite>Karl Marx, *Critique of Hegel's Philosophy of Right*</cite>

TODO

 - `(expt x y)` is tricky:
   - When `y` is `natural` we get the same type as `x` (assuming it's closed
     under multiplication, which all of our types are)
   - When `y` is `integer` we get `rational` numbers or below
   - When `y` is `rational` we get `algebraic` and below
   - Probably don't want anything more general than `rational`, since their
     semantics involve logarithms, which I don't know a normal form for
   - Unclear what's the most general `x` we can support, when `y` is `rational`:
     - I think `algebraic` is OK; roots of products seem fine, roots of sums
       don't easily rewrite, which I *hope* means they're normalised, right?
     - Roots become more, ahem, *complex* when dealing with `complex`, and other
       hypercomplex numbers; since sums can cancel-out when squared.
     - Seems reasonable to return `complex` when rooting a negative
     - If there's a symbolic normal form for other `geometric` numbers it would
       be nice to use that
     - Fractional powers of a `polynomial` should be representable using
       `algebraic-expression`, I think?
     - Note: don't confuse fractional powers of a `polynomial` with "roots"
       (substitutions for the indeterminate which evaluate to `0`)
 - Normalising `algebraic` requires factorising. That's slow, but is "zero cost"
   in the sense we'll never do it unless someone starts taking roots...
 - Is normalising as simple as factoring, until we get a fixed-point of
   X = sums-of-products-of-powers-of X?
 - How to read and write powers of sums? Would like to avoid parentheses...
