---
title: "Ivory: Ratioed"
---

Not all of the values we want Ivory to represent can be expressed as a
[sum-of-products](sums_and_products.html): some require a *ratio* of two
sums-of-products, which we'll write with a slash `/`{scheme}, like
`(/ a b)`. This structure will always contain two sum-of-products expressions:
we'll call the first the *numerator* (which is `a`{.scheme}, in that example)
and the second the *denominator* (`b`{.scheme} in that example). The denominator
should never be `(+)`{.scheme} (Ivory's representation of zero).

Ratios are a powerful numerical representation, but they introduce some
redundancies which must be normalised.

### Division by one ###

A ratio with denominator `(×)`{.scheme} (which is Ivory's representation of the
number one) is equivalent to its numerator. Hence the normal form for all Ivory
numbers will be a ratio-of-sums-of-products: if a number is not already such a
ratio, we will normalise it by making it the numerator of a ratio, with a
denominator of `(+ (×))`{.scheme}.

### Common factors ###

If a particular value appears in every product in both the numerator and
denominator of a ratio, then it is a "common factor" which does not affect the
result and can hence be removed. Symbolic factors can be found by simply
checking for their presence or absence in the elements of a product. Since we
represent `natural` numbers in place-value form, an explicit factorisation
algorithm must be used.

### Rationalising denominators ###

Ratios involving symbolic values can be represented in multiple ways: some with
symbolic values in the numerator, some with them in the denominator, and some
with both. For example, a negative ratio may have a factor of negative one in
either the numerator or denominator. We will avoid this ambiguity by choosing
the form which has the fewest symbolic values in the denominator. This is called
[rationalising the
denominator](https://www.wikihow.com/Rationalize-the-Denominator) when applied
to fractions involving radicals. It relies on the following laws:

 - A ratio `(/ n n)`{.scheme} is equal to `(×)`{.scheme}, i.e. to the number
   one (whenever `n` is non-`zero`).
 - `(×)`{.scheme} is the
   [identity value](https://en.wikipedia.org/wiki/Identity_element) for
   multiplication, i.e. multiplying by `(×)`{.scheme} leaves any number
   unchanged.
 - Hence multiplication by `(/ n n)`{.scheme} leaves any number unchanged (for
   non-`zero` `n`)

This fact can be used to introduce extra factors into the numerator and
denominator of a ratio. We can eliminate symbolic factors from the denominator
through an appropriate choice of `n`. For example, given a ratio like
`(/ 3 i)`{.scheme} (where `(= (× i i) (- (×)))`{.scheme}), we can eliminate `i`
from the denominator by multiplying the ratio by `(/ i i)`{.scheme}:

```scheme
(= (/ 3 i)
   ; Multiply by (/ i i) to eliminate i from the denominator
   (× (/ 3 i) (/ i i))
   (/ (× 3 i) (× i i))
   (/ (× 3 i) (- (×)))
   ; Multiply by (/ (- (×)) (- (×))) to eliminate (- (×)) from the denominator
   (× (/ (× 3 i) (- (×))) (/ (- (×)) (- (×))))
   (/ (× 3 i (- (×))) (× (- (×)) (- (×))))
   (/ (× 3 i (- (×))) (×)))
```
