---
title: "Ivory: Indeterminates"
packages: ['racketWithRackCheck']
---
TODO

 - Adjoin with an indeterminate `v`, which is commutative
 - Start with `univariate-monomial`, i.e. individual powers `v^0`, `v^1`, `v^2`,
   `v^3`, etc.
 - These don't collapse since there are no identities like `d^2 = 0`, etc.
 - Technically an infinite-dimensional vector space. Is this useful to our
   implementation?
 - Introduce other indeterminates `v1`, `v2`, etc. to get general monomials
 - Introduce "degree", and `homogeneous` polynomials which sum monomials of the
   same degree
 - Useful for projective geometry, etc.

## Beyond the number lines ##

In the `geometric` level we found values which do not appear on a simple "number
line", but instead inhabit a larger space full of interesting geometrical
structure such as rotations. This structure could be neatly described using our
existing notions of arithmetic, if we allow some extra values like `i₀`, `d₂`,
etc.

## Symbols without value ##
