---
title: The Metric Red Herring
---

This began as a companion to my post about
[improving our units](../../blog/2020-05-22-improving_our_units.html), which
discusses some problems with the metric system, SI and related ideas. This post
was a stand-alone argument that advocates of metric are often doing it for the
wrong reasons.

Both are now part of a larger [collection of suggestions to improve and simplify
measurement, notation, numeracy, etc.](./index.html)

## The Powers-of-Ten Red-Herring ##

Most discussions about metric versus imperial units seem to dwell on one
specific aspect of metric: that units are related by powers of 10. For example:

 - 100cm = 1m
 - 10mm = 1cm
 - 1kN = 1000N

However, this is a red herring: those examples have *the same units* on each
side! They only differ in how they factor the multiple; solving these equations
reveals the definition of those factors:

The first tells us that `c` ("centi") is a hundredth:

```
100cm = 1m
100c  = 1
   c  = 1/100
```

The second tells us that `m` ("milli") is a thousandth:

```
10mm = 1cm
10m  = 1c
10m  = 1/100
  m  = 1/1000
```

The third tells us that `k` ("kilo") is a thousand:

```
1kN = 1000N
 k  = 1000
```

Notice that these are just statements about numbers: the units we wrote in each
case are irrelevant, since they immediately cancelled-out! We could have done
the same with inches, pounds, etc. There's also nothing special about ten: there
are [convenient factors for many bases](prefix_factors.html), e.g. 3Kim = 3072m.

## Metric is Minimal ##

Focusing on "conversions" like 1km = 1000m obscures a much more important
feature of metric: there is only *one* unit of distance. Likewise there is
only *one* unit of force, *one* unit of pressure, and so on for each distinct
[dimension](https://en.wikipedia.org/wiki/Dimensional_analysis).

Quantities 'expressed in kilometres' are still expressed in metres, since
kilometres are just multiples of metres. A distance like "5m" is expressed in
metres (five of them, since there's a "5" which means five). A distance like
"7km" is *also* expressed in metres (seven thousand of them, since there's a "7"
which means seven and a "k" which means thousand). "centi", "milli", "femto",
"kilo", "giga", etc. are just generic ways to abbreviate large and small
numbers.

If we use a multiple which isn't base 10, like "two dozen metres", that's still
a metric distance; we haven't invented a new system of units with base 12;
"dozen" is just a generic linguistic device meaning "twelves". Compare this to a
foot containing a dozen inches: if we ask a baker for a dozen buns we'll get 12
buns; if we ask for a foot of buns we'll a line of buns about a third of a metre
long. This is because "foot" is *not* a generic multiplier: it is specific to
distance, and it is independent of other distance units (e.g. we don't need to
say "a foot of inches"). The same applies to "inch", "mile", etc. hence they are
all separate units, not just multipliers. In contrast, "metres" and "dozen
metres" are not separate units, and neither are "metres" and "centimetres".

(Note that there is a slight wrinkle here, since "kilo" on its own is taken to
mean "kilograms"; this is not too important for this claim, but I explain why
the kilogram is problematic in the "Problems with SI" section of
[the companion post](improving_our_units.html)!)

I think this is such a profound advantage that many (most?) people, even those
born and raised with metric, never grasp it explicitly. After all, why argue
that "power-of-ten conversions are easier" when we could go further and say
"there's nothing to convert between"; it can't get any easier than that!

## Metric is Coherent ##

In [a 'coherent' system of units](coherence.html), we can combine our units
using multiplication and division, and alway get another unit of our system. The
most obvious example is speed, which is the same as a distance divided by a
time. There are imperial units of speed like the knot, but it's more common to
use the "distance over time" form like "miles per hour", or "metres per second"
in metric. Similarly for pressure, which is often measured in "pounds per square
inch" or "Newtons per square metre".

We can do the same thing with any dimensions we like. For example, applying a
force (e.g. Newtons or pounds) over a distance (metres or feet) takes a certain
amount of energy (Joules or calories); hence we can express distances in units
of "energy over force". We can think of this as how far we could push with a
unit of force, before we use up the given energy.

These 'derived units' will be some multiple of the 'normal' units for that
dimension, requiring a conversion factor if we want to convert a number between
the two forms. Here are some conversion factors for common imperial units:

<div class="summarise">
 <span class="summary">
  Conversion factors between various imperial distance units.
 </span>

Quantity            Inches  Feet     Miles    Calories per pound Calories per ounce
------------------- ------  -------  -------  ------------------ ------------------
1 inch              1       0.083    2x10^-5^ 0.03               0.43
1 foot              12      1        2x10^-4^ 0.32               5.18
1 mile              6x10^4^ 5280     1        1711               3x10^4^
1 calorie per pound 37.03   3.09     6x10^-4^ 1                  16
1 calorie per ounce 2.31    0.19     4x10^-5^ 0.06               1

</div>

There are conversion tables like this for many other combinations of units, e.g.
to find how many slug feet per square hour are in a stone. If the order of the
units is the same in the rows and columns then the main diagonal will always be
1. For the remaining numbers, we only need to remember one of the 'triangles'
(upper-right or lower-left), since we can get the other by 'reflecting' the
positions across the diagonal and taking the reciprocal of the numbers. (Or you
can [look them up](https://lmgtfy.com/?q=+1+calorie+per+pound+in+miles+!g&s=d)
like I did, rather than cluttering your brain with obsolete junk!)

The metric equivalent doesn't have the inch/foot/mile or ounce/pound
redundancies, so the table is much smaller, and hence easier to memorise:

<div class="summarise">
 <span class="summary">
  Conversion factors between metric distance units.
 </span>

Quantity           Metres Joules per Newton
------------------ ------ -----------------
1 metre            1      1
1 joule per newton 1      1

</div>

The conversion factors here are all 1. This is not a coincidence, it is *by
design*! [1 Joule is *defined as* 1 Newton
metre.](https://www.bipm.org/utils/common/pdf/si_brochure_8_en.pdf) Likewise:

```
1 Watt = 1 Joule per second
       = 1 Hertz Joule
       = 1 Hertz Newton metre
       = 1 Hertz Pascal cubic metre
       = 1 Hertz square Coulomb per Farad
       = 1 Newton Coulomb per Tesla Farad metre
       = 1 Newton Joule per Tesla Coulomb metre
       = 1 Newton Volt per Tesla metre
       = 1 Amp Volt
       = 1 Watt
```

This is a *huge* advantage to using metric, which I rarely/never see brought up
in discussions. I'm not sure whether this is because it's subconsciously taken
for granted (like the one-unit-per-dimension feature) or whether it's just used
less frequently in "real life" (e.g. day-to-day estimating, rather than explicit
engineering calculations). Either way, I think this should be celebrated more!

In particular, these conversions are based off known scientific laws. For
example Newton's second law of motion, usually written `F = ma`, tells us that
multiplying a mass by an acceleration results in a force. This is actually a
statement of *proportionality*, e.g. doubling the mass will double the force; to
get an equation we need a "constant of proportionality", which is an arbitrary
scaling factor which we usually write as `k`, so the general form of Newton's
second law should really be expressed as `F = kma`. Metric units are *defined
such that* these scaling factors are 1, which gives us simple equations without
having to remember a bunch of proportionality constants (i.e. those shown in the
tables above).

## Exposing My Lies ##

I have to admit that the above is slightly inaccurate, for the purposes of
getting across my way of thinking. The first major point to clarify is that when
I say "metric" I'm actually referring mostly to
[the SI system](https://en.wikipedia.org/wiki/International_System_of_Units),
which differs a little from the metric units that are commonly used day-to-day.
In particular, metric often uses extra, redundant units which are not part of
SI, including the litre, tonne and hour.

Next, I claimed above that prefixing a unit changes the associated number rather
than the unit, e.g. "2km" is 2000 in the unit of metres, rather than 2 in the
unit of kilometres. In fact, [the SI
definition](https://www.bipm.org/utils/common/pdf/si_brochure_8_en.pdf)
explicitly states that prefixing a unit with a multiplier, like "kilometre",
gives us a new, "derived" unit. This is important for resolving otherwise
ambiguous quantities like "3cm^3^": according to SI, this is 3(cm)^3^ =
0.000003m^3^, whereas treating prefices in the way I describe would give
3c(m^3^) = 0.03m^3^. Similar problems arise when dividing, e.g. "per kilometre".

Whilst the SI method is well-defined, it still places a mental burden on the
user. What's especially annoying is that the SI rules for units are opposite to
the usual algebraic rules for multiplication and exponentiation, where
ab^3^ = a(b^3^)

I cover these in more depth in
[the companion post](improving_our_units.html), but I think the best
thing to do in these situations is stick to the base unit for that dimension
(e.g. "cubic metre" or "per metre"), apply prefix multipliers if they are
unambiguous; or otherwise add
[explicit parentheses](https://en.wikipedia.org/wiki/S-expression).

## Take Aways ##

Whilst the metric (or SI) system
[isn't perfect](improving_our_units.html), it's also much better than
those hodge-podges of historical baggage known as imperial units. The most
obvious argument against imperial units is that [there are so many, related in
arbitrary ways](https://www.youtube.com/watch?v=r7x-RGfd0Yk); but
that's a bit of a cheap shot, since few people make regular use of barleycorns,
fathoms or leagues. Likewise the common argument *against* imperial, that powers
of ten make arithmetic easier, is shallow at best, and irrelevant at worst.
Instead, the two *real* advantages of metric (or SI) are having one unit per
dimension, and requiring no conversion factor when combining dimensions.

I'm a firm believer that seemingly-innocuous complications, like those found in
imperial units of measurement, are in fact significant risks; they impede
learning, potentially turning people away from areas like maths and science; and
their [compounding, confounding behaviour on the large
scale](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter) constrains what we're
capable of achieving as a species.

Every small "gotcha" can be pre-empted on its own, but it takes knowledge and
experience to do so, and some small effort every time. As such "minor" issues
combine together, they can quickly overflow our limited mental capacity,
making it naïve and irresponsible to think their individual avoidability can
hold in general. (As a programmer, such
[seemingly](https://en.wikipedia.org/wiki/Strong_and_weak_typing)
[minor](http://wiki.c2.com/?CeeLanguageAndBufferOverflows)
[problems](https://en.wikipedia.org/wiki/Code_injection) are quite widespread,
and even skilled experts often slip up now and then!)

The only way to combat such unnecessary complication is by an aggressive pursuit
of simplicity. Irrelevant details, unwanted degrees of freedom and unnecessary
asymmetries only act to slow us down and trip us up. Imperial units have
infected our collective mind for so long that we're often unable to see the
simplicity that metric provides: we used to waste so much effort converting
between redundant imperial units that, when confronted with a single metric
equivalent, we started treating multiples as if they were different units, just
to make it more familiar.

The other advantage, of combining dimensions, is alien to many, despite the
prevalence of examples like "miles per hour" and "pounds per square inch".
Presumably this is due to how horrible it is to convert between imperial units
in this way. It might even be the case that quantities like "miles per hour" and
"pounds per square inch" are acceptable precisely because there's no expectation
that they be convertible to any existing units (other than their constituents,
like "miles" and "hours" for "miles per hour"). This mentality might explain why
someone thought it was a good idea to invent monstrosities like "kilowatthours",
rather than just sticking a "mega" prefix on to the Joule!

In any case, we need to embrace the simplicity of metric; grok what it tells us
about the nature of measure and dimension; and use the saved mental effort to
tackle bigger, harder problems.
