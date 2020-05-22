---
title: The Metric Red Herring
---

Most discussions about metric versus imperial units seem to dwell on one
specific aspect of metric: that units are related by powers of 10. For example:

 - 100cm = 1m
 - 10mm = 1cm
 - 1kN = 1000N

However, this is a red herring!

## Metric Isn't Powers-of-Ten ##

The qualifiers "centi", "kilo", "milli", etc. are not unique to metric. If we
wanted to, we could use them with imperial units too. For example it's perfectly
correct (if a little unorthodox) to say that there are 100 centifeet in 1 foot
and 1000 pounds in 1 kilopound. A listener might find it strange, but they could
figure out what you meant.

"centi", "milli", "femto", "kilo", "giga", etc. are just generic ways to
abbreviate large and small numbers. If we say "two dozen metres", we haven't
invented a new base-12 system of units, since "dozen" is a generic multiplier.
Compare this to a foot containing a dozen inches: if we ask a baker for a dozen
buns we'll get 12 buns; if we ask for a foot of buns we'll a line of buns about
a third of a metre long. This is because "foot" is a specific unit of distance,
not a linguistic device to indicate a multiple; hence "inches" and "feet" are
separate units, whilst "metres" and "dozen metres" aren't.

The "powers of 10" in metric come solely from the common usage of these
prefices. There are equivalent prefices for powers of 2, where "kibi" is 2^10^
(symbol "Ki"; roughly a thousand), "mibi" is 2^20^ (symbol "Mi"; roughly a
million), etc. These are used almost exclusively for data sizes and rates, like
an SSD with a 1TiB capacity (one tibibyte, or 2^40^ bytes). Yet it's perfectly
correct (if a little unorthodox) to say that 1Mim = 1,048,576m or that there
are one thousand and twenty four gallons in one kibigallon.

## Multiples Aren't Units ##

Focusing on "conversions" like 1km = 1000m obscures a much more important
feature of metric: there is only *one* unit of distance. Likewise there is
only *one* unit of force, *one* unit of pressure, and so on for each distinct
[dimension](https://en.wikipedia.org/wiki/Dimensional_analysis).

Quantities 'expressed in kilometres' are still expressed in metres, since
kilometres are just multiples of metres. A distance like "5m" is expressed in
metres (five of them, since there's a "5" which means five). A distance like
"7km" is *also* expressed in metres (seven thousand of them, since there's a "7"
which means seven and a "k" which means thousand).

I think this is such a profound advantage that many (most?) people, even those
born and raised with metric, never grasp it explicitly. After all, why argue
that "power-of-ten conversions are easier" when we could go further and say
"there's nothing to convert between"; it can't get any easier than that!

## Metric Conversions Multiply By One ##

Metric only has one unit for each dimension, but some combinations of dimension
are equivalent to others. The most obvious example is speed, which is the same
as a distance divided by a time. There are imperial units of speed like the
knot, but it's more common to use the "distance over time" form like "miles per
hour", or "metres per second" in metric. Similarly for pressure, which is often
measured in "pounds per square inch" or "Newtons per square metre".

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
1 Watt = 1 Volt Amp
       = 1 Joule per second
       = 1 Newton metre per second
       = etc.
```

This is a huge advantage to using metric, which I rarely/never see brought up in
discussions. I'm not sure whether this is because it's subconsciously taken for
granted (like the one-unit-per-dimension feature) or whether it's just used less
frequently in "real life" (e.g. day-to-day estimating, rather than explicit
engineering calculations). Either way, I think this should be celebrated more.

In particular, these conversions are based off known scientific laws. For
example Newton's second law of motion, usually written `F = ma`, tells us that
multiplying a mass by an acceleration results in a force. This is actually a
statement of *proportionality*, e.g. doubling the mass will double the force; to
get an equation we need a "constant of proportionality", which is an arbitrary
scaling factor which we usually write as `k`, so the general form of Newton's
second law should really be expressed as `F = kma`. Metric units are defined
such that these scaling factors are 1, which gives us simple equations without
having to remember a bunch of proportionality constants (i.e. those shown in the
tables above).

## Remaining Problems ##

There are some problems with what I've said above, with the metric system itself
and with how it's used.

### Exposing My Lies ###

The first major point to clarify is that when I say "metric" I'm actually
referring mostly to the SI system, which differs a little from the metric units
that are commonly used day-to-day (more details below).

Next, I claimed above that prefixing a unit changes the associated number rather
than the unit, e.g. "2km" is 2000 in the unit of metres, rather than 2 in the
unit of kilometres. In fact, [the SI
definition](https://www.bipm.org/utils/common/pdf/si_brochure_8_en.pdf)
explicitly states that prefixing a unit with a multiplier, like "kilometre",
gives us a new, "derived" unit. This is important for resolving otherwise
ambiguous quantities like "3cm^3^": according to SI, this is 3(cm)^3^ =
0.000003m^3^, whereas treating prefices in the way I describe would give
3c(m^3^) = 0.03m^3^. Similar problems arise when dividing, e.g. "per kilometre".

I think the best thing to do is stick to the appropriate base unit (e.g. "cubic
metre" or "per metre"), then choose whichever multipliers make life easier. If
the base units are annoyingly big or small, try sticking a prefix on the front;
if a prefix makes things look ambiguous (e.g. like the cm^3^ example) then drop
it in favour of something else.

### Problems with Metric ###

The actual metric system, as it's used in day-to-day life, has a few problems.
The following problems are solved by sticking to SI units:

 - The litre is a stupid unit: it's 1 milli-cubic-metre. The only reason to
   avoid cubic metres seems to be the unwieldy name, but other languages have
   solved this with names like "stere" and "kuub". The kuub might seem rather
   large for everyday quantities like drinks, but it's already common to use
   phrases like "700 mill bottle" (700ml = 0.7l). The same works with kuubs,
   with a litre bottle becoming "one mill" and 0.7l becoming 700 microkuub (a
   "700 micro" bottle).

 - It's common for metric to use hours (1 hour = 3600 seconds), e.g. km/h or
   "kilometres per hour". This gives us two units of time, when it would be
   better to use a prefix multiplier. Note that I'm not claiming that [decimal
   time](https://en.wikipedia.org/wiki/Decimal_time) would be better; simply
   that it would be better to use a multiple of seconds rather than a whole
   different unit. Such a system would actually be the inverse of the [current
   system of
   hours/minutes/seconds](https://en.wikipedia.org/wiki/Minute#History); where
   the hour is the base unit, a
   [sixtieth](https://en.wikipedia.org/wiki/Sexagesimal) of an hour is a "pars
   minuta prima" (AKA "minute", but should really be "prime"), a sixtieth of
   that is a "pars minuta secunda" (AKA "second"), etc. Perhaps the unit
   "second" could also be renamed when used as a base unit, to avoid the
   implication that it is derived.

 - Metric temperatures are usually given in Celsius or Centigrade (those are the
   same thing!), which can be positive or negative. Since there is an
   ["absolute zero"](https://en.wikipedia.org/wiki/Absolute_zero) temperature,
   we can avoid
   [false numbers](https://en.wikipedia.org/wiki/Negative_number#History) by
   choosing absolute zero as our starting point. If we count "degrees Celsius
   starting from absolute zero", we get the Kelvin scale, which SI uses.

### Problems with SI ###

The SI system avoids the extraneous units of metric like litres and km/h, and
unnecessary degrees of freedom like the zero-point of Celsius. The most obvious
problem remaining with SI is that its base unit of mass is the "kilogram".
Whilst a kilogram equals one thousand grams, as we would hope, the *definition*
is backwards: the gram is defined as one thousandth of a kilogram, AKA a
millikilogram.

Within the dimension of mass this is merely silly; the real problem arises when
we start combining dimensions. In the example of Newton's second law above, it
is true that in SI units the equation `F = ma` holds, i.e. the constant of
proportionality is 1. However, the `m` must be in the base unit of *kilograms*;
if we want to use grams we need a constant of proportionality of 1/1000. This is
why I strategically chose to avoid using mass in most of the examples above!
This is purely a naming issue, since we can just rename the "kilogram" to
something without an ambiguous prefix; for example, it used to be called the
["grave"](https://en.wikipedia.org/wiki/Kilogram#Name_and_terminology). This
would replace the gram with the milligrave, which explains where the 1/1000
factor comes from.

Note that there's an alternative approach, where we treat the gram as the base
unit and hence the kilogram's prefix works properly. Such a system *was* widely
used, and is now know as the ["CGS"
system](https://en.wikipedia.org/wiki/Centimetre%E2%80%93gram%E2%80%93second_system_of_units).
Unfortunately that system uses the "centimetre" as its base unit of length, and
hence suffers the same problem, except in the length dimension rather than mass.

The problem I mentioned above, where units like "cm^3^" may be misinterpreted,
is resolved in SI by giving prefices higher precedence than exponents (e.g. the
"c" applies first, then the "^3^" is applied after). This allows derived units
to creep into our calculations, and is also opposite to the usual rules of
multiplication and exponents, e.g. algebraically we would say that
ab^c^ = a(b^c^). My preferred solution to any potential ambiguity is to [use
parentheses](https://en.wikipedia.org/wiki/S-expression); I would try to avoid
writing something like "3cm^3^", in favour of "3c(m^3^)" or "3(cm)^3^"; and I
would also avoid the latter, since it's in derived units.

### Problems with Units ###

There are alternatives to SI called ["natural
units"](https://en.wikipedia.org/wiki/Natural_units). Whilst the metric and SI
systems take their derived units from physical equations like `F = ma`, natural
units go one step further and take their base units from physical constants like
the speed of light (in a vacuum). Using natural units can make physical
calculations much easier, for example the equation E=mc^2^ becomes E=m if the
speed of light is a base unit.

There are three problems with switching to natural units:

 - There isn't just one set of natural units, so we would need to pick one to
   standardise on. This isn't insurmountable.

 - Natural units can choose different dimensions for familiar quantities. For
   example, some set the speed of light to be dimensionless (rather than "length
   over time"). This requires changing other dimensions to maintain consistency,
   which could make for a confusing system. This might not be too bad, but is
   important to keep in mind.

 - These units are very far removed from everyday scales, so would need some
   pretty powerful prefices (both big and small) to make them usable.

### Problems with Prefices ###

10 isn't a particularly good base for a number system. It's only divisible by 1,
2, 5 and 10, which obscures many patterns (i.e. those which don't have period 2
or 5) and overly-complicates the representation of otherwise 'simple' numbers
(like the 1Mim example above). The binary prefices ("kibi", "mibi", etc.) are
preferable in this regard, although their exponents (10, 20, ...) are rather
arbitrary; we only use them since they're close to more familiar powers of 10.

Commonly proposed alternatives to base 10 are
[base 2 (binary)](https://en.wikipedia.org/wiki/Binary_number),
[base 12 (dozenal)](https://en.wikipedia.org/wiki/Duodecimal) and
[base 60 (sexagesimal)](https://en.wikipedia.org/wiki/Sexagesimal). These are
[superior highly composite
numbers](https://en.wikipedia.org/wiki/Superior_highly_composite_number) (i.e.
they have many factors), and hence they can show patterns with more periods. For
example, sequences differing by 2, 3, 4 and 6 will show a pattern in their
digits ("dozits"?) when written in dozenal. Dozenal also makes it easy to
[count using one's fingers](https://en.wikipedia.org/wiki/Duodecimal#Origin).
Binary is also easy to [count on one's
fingers](https://en.wikipedia.org/wiki/Finger_binary), but the dearth of digits
("bits") can lead to very long, unwieldy numbers.
[Hexadecimal (base 16)](https://en.wikipedia.org/wiki/Hexadecimal) is a common
way to avoid this problem of binary, whilst still following similar patterns.

Another problem with [current unit
prefices](https://en.wikipedia.org/wiki/Metric_prefix#List_of_SI_prefixes) is
that they only increase in fixed multiples (of 1000), e.g.
1Gm = 1000Mm = 1000000km = 1000000000m. Such prefices act in a similar way to
[unary](https://en.wikipedia.org/wiki/Unary_numeral_system) (or perhaps
[Roman numerals](https://en.wikipedia.org/wiki/Roman_numerals)). This doesn't
scale, requiring the rapid invention of
[new names](https://en.wikipedia.org/wiki/Unit_prefix#Unofficial_prefixes).

This problem is likely inherited from the [naming system used for large
numbers](https://en.wikipedia.org/wiki/Names_of_large_numbers), where "million"
is a thousand thousands, "billion" is a thousand millions, "trillion" is a
thousand billions, etc. Just like place-value numerals make more efficient use
of digits than unary, we can make more efficient use of names by introducing
them logarithmically. In base ten we invent a new name for ten tens: the
"hundred". With this new name we can count up to 99,9,9 ("ninety nine hundred
and ninety nine"), but our next name (the "thousand") appears after only 9,9,9;
far too early! If the thousand were a hundred hundreds, we could then count up
to 9999,99,9,9 ("ninety nine hundred and ninety nine thousand, ninety nine
hundred and ninety nine") before needing a new name (e.g. the "million"); the
million currently appears after 99,99,9,9. The next name ("billion") currently
appears after 9,9999,99,9,9, but we only need it once we reach
99999999,9999,99,9,9 (a million millions, using our redefinition of million);
the trillion would only be needed once we reach a billion billions, and so on.
Note that each name doubles the number of digits we can reach, rather than
merely adding three ([or
six](https://en.wikipedia.org/wiki/Long_and_short_scales)). Such names have a
clear relation to binary (powers of two), and also remind me of [factoradic
numbers](https://en.wikipedia.org/wiki/Factorial_number_system). If this were to
be adopted, it would be a much better idea to invent new names rather than
trying to redefine the current terms!

## Closing Thoughts ##

Some of the above is fanciful, and other parts are purposefully provocative.
The most obvious argument against imperial units is that [there are so many,
related in arbitrary ways](https://www.youtube.com/watch?v=r7x-RGfd0Yk); but
that's a bit of a cheap shot, since few people make regular use of barleycorns,
fathoms or leagues. The two *real* advantages of metric (or SI) are having one
unit per dimension, and requiring no conversion factor when combining
dimensions.

I'm a firm believer that seemingly-innocuous complications, like those found in
imperial units of measurement, are in fact significant risks; they impede
learning, potentially turning people away from areas like maths and science; and
their [compounding, confounding
behaviour](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter) on the large
scale constrains what we're capable of achieving as a species.

Every small "gotcha" can be pre-empted on its own, but it takes knowledge and
experience to do so, and some small effort every time. As such "minor" issues
combine together, they can quickly overflow our limited mental capacity,
making it naïve and irresponsible to think their individual avoidability can
hold in general. (As a programmer, such
[seemingly](https://en.wikipedia.org/wiki/Strong_and_weak_typing)
[minor](http://wiki.c2.com/?CeeLanguageAndBufferOverflows)
[problems](https://en.wikipedia.org/wiki/Code_injection) are quite widespread,
and even skilled experts often slip up now and then!)

Whilst I don't think we'll see speedometers denoting logarithmically-scaled
dozenal divisions of lightspeed any time soon, I think there are some definite
steps we can take to improve our use of units.

 - Switching to metric would be the easiest first step, and would bring the
   two main benefits: one unit per dimension (modulo the litre and hour
   inconsistencies), and trivial conversion between dimensions (modulo the
   kilogram inconsistency). Most of the world is here already, although the UK
   needs to sort out its road network (one way to do this would be replacing all
   car usage with public transport and bicycles, but that's a whole other
   issue!). Pro tip for those not used to metric distances: whenever you see
   "metre", just think "yard" and you're basically done!

 - Kelvin is a well-known and widely-used unit. Whilst day-to-day usage might be
   a bit unfamiliar, there's no major problem with using it in this way right
   now.

 - We can avoid two more inconsistencies with metric by renaming the litre to a
   cubic-metre equivalent like the millikuub, and renaming the kilogram, e.g.
   back to the grave. This is quite low-effort, since the quantities would
   remain the same, only the terminology would change. Anyone can start doing
   this today (accompanied by old-style conversions, until they catch on).
   There's little risk of unaccompanied quantities becoming unfathomable, since
   the names "kuub" and "grave" are pre-existing (albeit foreign and archaic,
   respectively).

 - The time inconsistency can be avoided by using new names for "minutes" and
   "hours", as sexagesimal multiples of seconds (although this may annoy
   etymologists!). Again, since the quantities remain the same, we can start
   doing this right now; although some agreement on what to call these  prefices
   would be useful.

 - More speculatively, we can switch from base-10 prefices "kilo", "mega", etc.
   to base-2 "kibi", "mibi", etc. since they're close enough, and the base is
   more sensible. Again, we can do this today, and the meaning would be mostly
   clear (perhaps listeners would think we have a speech impediment, but they'd
   get the right order of magnitude).

More long-term it would be nice to switch to dozenal and/or hexadecimal, but
that has a big chicken-and-egg problem. In particular, re-using the digits 0-9
would make existing decimal representations ambiguous. It might be easier to
learn and teach mathematics in these systems, due to more patterns being
present, but those skills would be harder to apply in the "real world" where
base 10 is prevalent.
