---
title: Quick wins for improving units
packages: [ 'mathml' ]
---

Some of the ideas described in this project are more aspirational than
practical, and need adoption by many people in order to be worth switching. In
contrast, the entries on this page can be followed *right now*, with instant
benefits!

## Units ##

### Replace imperial with metric ###

Imperial units differ between countries, have arbitrary conversion factors, and
lots of redundancy. Metric is the same in every country, and doesn't require
conversions (it is *coherent*).

#### Use metres instead of yards ####

Those stuck with imperial for compatibility (e.g. in the USA) may find it useful
to measure distances in yards rather than feet: since a yard is roughly the same
size as a metre, this can build intuition for metric, and rough estimates can be
used interchangably.

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
per turn; a quantity we represent with the symbol τ. Writing our angles with a τ
suffix, like ¼τ, works for *both* of these units:

 - We can interpret τ as 'turns', similar to how we interpret ° as 'degrees'.
   Hence ¼τ can be read as "a quarter turn".
 - Alternatively, we can interpret τ as a conversion factor between radians and
   turns, with a value around 6.28… radians per turn. In which case ¼τ can be
   read as "a quarter of the radians in a turn", which is about 1.57… radians.

Both of these interpetations are equivalent/interchangable: the syntax we write
down is the same for both, how we choose to interpret them is a matter of taste
and convenience.

### Replace π with τ/2 ###

The constant π (around 3.14…) is the number of radians in half a turn. Since we
already have the constant τ for the number of radians in a turn, we can replace
all occurrences of π with τ/2. Whilst the factor of ½ seems annoying, having it
appear explicitly in a formula seems preferable to having it be *implicit* in
the definition of π (in fact it is common for formulas to contain 2π, which have
the opposite problem!)

### Add negatives instead of subtracting ###

Subtraction is redundant and doesn't behave as nicely as addition: in particlar,
rearranging subtractions will change their result (it's not 'commutative' or
'associative'), e.g. $(2 - 3) - 7 = 8̅$, but $2 - (3 - 7) = 6$.

Subtraction requires negative numbers (like the $8̅$ above); in which case, we
might as well use them as *inputs* too. That way we can always use addition,
which is commutative and associative, e.g.
$(2 + 3̅) + 7̅ = 2 + (3̅ + 7̅) = 3̅ + 2 + 7̅ = 8̅$
