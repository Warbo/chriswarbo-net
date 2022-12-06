---
title: Quick wins for improving units
---

Some of the ideas described in this project are more aspirational than
practical, and need adoption by many people in order to be worth switching. In
contrast, the entries on this page can be followed *right now*, with instant
benefits!

## Units ##

### Replace imperial with metric ###

Imperial units differ between countries, have arbitrary conversion factors, and
lots of redundancy. Metric is the same in every country, and doesn't require
conversions (it is 'consistent')..

#### Use metres instead of yards ####

Those stuck with imperial for compatibility (e.g. in the USA) may find it useful
to measure distances in yards rather than feet: since a yard is roughly the same
size as a metre, this can build intuition for metric, and rough estimates can be
used interchangably. (Using kiloyards instead of miles, and centiyards instead
of centimetres is lessof a 'quick win'!)

### Use the SI subset of metric ###

The metric system has some redundant units like hectares, litres and tonnes.
Avoid them, and stick to the subset defined by SI (in those cases: square
metres, cubic metres and kilograms, respectively).

### Measure angles in turns ###

Turns are a direct, intuitive unit for rotation. They are more natural than
arbitrary alternatives like degrees (1/360th of a turn) or gradians (1/400th of
a turn). For example we can say 'a quarter turn' rather than '90 degrees'.

### Write turns using τ ###

Radians are another natural unit of angle, where there are around 6.28… radians
per turn; a quantity we represent with the symbol τ (also equal to 2π). Writing
our angles with a τ suffix, like ¼τ, works for *both* of these units:

 - We can interpret τ as 'turns' (similar to interpreting ° as 'degrees'); in
   this case a quarter of a turn
 - Alternatively, we can interpret τ as the constant 6.28… radians per turn; in
   which case we're multiplying it by ¼ of a turn, to get about 1.57… radians.

Both of these interpetations are equivalent/interchangable; we can use whichever
is most convenient, and switch between them as desired.

### Replace π with τ/2 ###

The constant τ is equal to 2π, so any quantity or formula using one can easily
be converted to use the other. Since we're already using τ for turns, we never
need to use π.

### Add negatives instead of subtracting ###

Subtraction is redundant and doesn't behave as nicely as addition: in particlar,
rearranging subtractions will change their result (it's not 'commutative' or
'associative'), e.g. $(2 - 3) - 7 = \bar{8}$, but $2 - (7 - 3) = \bar{2}$.

Subtraction requires negative numbers (like the above results); in which case,
we might as well use them as *inputs* too. That way we can always use addition,
which has nicer behaviour, e.g.
$(2 + \bar{3}) + \bar{7} = 2 + (\bar{7} + \bar{3}) = bar{8}$
