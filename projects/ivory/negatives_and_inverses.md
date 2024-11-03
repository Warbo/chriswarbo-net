---
title: "Ivory:: Negatives And Inverses"
packages: ['racketWithRackCheck']
---

Negatives can be represented using products, if we adjoin -1.

Inverses can be lifted to the top level: one big fraction (rational expression),
a/b, where a and b are sums of products (polynomials). We can probably allow
fractional powers of naturals too, but not of arbitrary expressions.

Having each expression be a fraction lets us avoid special symbols for each
inverse value.

Probably best to have ^ be a function, which spits out appropriate expressions;
rather than a constructor appearing in the expression format. What powers do we
want?
 - (^ n m) for arbitrary n and natural m can be expanded into a product, e.g.
   (^ n 5) = (/ (× n n n n n) 1)
 - (^ n -1) for arbitrary n is an inverse, just flip its fraction, e.g.
   (^ (/ a b) -1) = (/ b a)
 - (^ n (× -1 m)) for arbitrary n and natural m: expand out and invert, e.g.
   (^ (/ a b) (× -1 3)) = (× (/ b a) (/ b a) (/ b a))
 - (^ n m) for arbitrary n and integer m, based on the above cases
 - (^ n m) for rational n and rational m: take inverse if needed, take root if
   needed, expand out if needed. I think that's a valid algorithm? Maybe
   property-check it against a real rational implementation.

PARSING
 - Parse unicode COMBINING OVERLINE
   - Allow negative indeterminates, GA units, etc. so long as they're completely
     overlined (i.e. `d₀` needs a combining character for the `d` and another
     for the `₀`)
 - Allow partially-negative literals
   - Interpret these as negative digits
 - Can we just parse by peeking for a COMBINING OVERLINE and, if coming up,
   consuming everything until we hit a char without a subsequent COMBINING
   OVERLINE?
   - Recursively parse every other character, and negate the result
   - Would need special-case to handle negative digits
 - Ambiguity for reciprocal of a negative, e.g. `3̅²̅` could mean
   `(= (- (expt 3 2)) 9̅)` or `(= (expt 3̅ 2̅) 1/9)`
   - The former would be the result of a simple every-other-character parser
   - The latter can be represented with `3̅⁻²`
