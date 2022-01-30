---
title: Improving our Units
---

This began as a companion to my post about
[the metric red-herring](../../blog/2020-05-22-metric_red_herring.html). That
post was a simple claim that the main benefit of the metric system is not the
"multiply by 10" argument that comes up over and over in 'metric versus
imperial' discussions. Instead, the two main benefits are that:

 - Each dimension has a single unit, e.g. length in metres, time in seconds,
   force in Newtons, etc.

 - Combining metric units with multiplication and division keeps us within the
   metric system, with no conversion factors. For example a Newton metre is a
   Joule, a Joule Hertz is a Watt, a Watt is a Volt Amp, etc.

This post discusses some problems with metric, the SI system, and our systems of
counting more generally; proposes some alternatives; and picks a few of those
that would be reasonably practical to employ.

Both are now part of a larger [collection of suggestions to improve and simplify
measurement, notation, numeracy, etc.](./index.html)

## Problems with Metric ##

The metric system, as it's used in day-to-day life, has a few problems. The
following problems are solved by sticking to SI units:

 - The "tonne" is just a megagram.

 - The litre is a stupid unit: it's 1 milli-cubic-metre. The only reason to
   avoid cubic metres seems to be the unwieldy name, but other languages have
   solved this with names like "stere" and "kuub". Cubic metres may seem quite
   large for everyday quantities like drinks, but it's already common to use
   phrases like "700 mill bottle" (700ml = 0.7litres). The same works with cubic
   metres, with a litre bottle becoming "one mill" and 0.7litres becoming "700
   micros".

 - Metric temperatures are usually given in Celsius or Centigrade (those are the
   same thing!), which can be positive or negative. Since there is an
   ["absolute zero"](https://en.wikipedia.org/wiki/Absolute_zero) temperature,
   we can avoid
   [false numbers](https://en.wikipedia.org/wiki/Negative_number#History) by
   choosing absolute zero as our starting point. If we count "degrees Celsius
   starting from absolute zero", we get the Kelvin scale, which SI uses.

## Problems with SI ##

The SI system avoids the extraneous units of metric like litres and km/h, and
unnecessary degrees of freedom like the zero-point of Celsius. The most obvious
problem remaining with SI is that its base unit of mass is the "kilogram".
Whilst a kilogram equals one thousand grams, as we would hope, the *definition*
is backwards: the gram is defined as one thousandth of a kilogram, AKA a
millikilogram.

Within the dimension of mass this is merely silly; the real problem arises when
we start combining dimensions. In
[the companion post](metric_red_herring.html) I use the example of
Newton's second law of motion `F = kma` where the constant of proportionality
`k` is equal to 1 in SI, due to the way its base units are defined. This is true
and good, but the base unit for `m` (mass) is the *kilogram*. If we want to use
grams, to avoid the sillyness of a prefixed base unit, we end up needing a
constant of proportionality `k = 1/1000`. This is why I strategically chose to
avoid using mass in most of my examples!

This is purely a naming issue, since we can just rename the "kilogram" to
something without an ambiguous prefix; for example, it used to be called the
["grave"](https://en.wikipedia.org/wiki/Kilogram#Name_and_terminology). This
would rename grams to milligraves, which explains where the factor of 1/1000
comes from.

Note that there's an alternative approach, where we treat the gram as the base
unit and hence the kilogram's prefix works properly. Such a system *was* widely
used, and is now know as the ["CGS"
system](https://en.wikipedia.org/wiki/Centimetre%E2%80%93gram%E2%80%93second_system_of_units).
Unfortunately that system uses the "centimetre" as its base unit of length, and
hence suffers the same problem, except in the length dimension rather than mass.

There is another problem with SI, that I mention in
[the companion post](metric_red_herring.html), which is that units
like "cm^3^" may be misinterpreted. SI resolves this by giving prefices higher
precedence than exponents (e.g. the "c" applies first, then the "^3^" is applied
after), but this is opposite to the usual rules of multiplication and exponents,
e.g. algebraically we would say that ab^c^ = a(b^c^).

This also allows derived units (e.g. cubic centimetres) to creep into our
calculations. My preferred solution to any potential ambiguity is to use
[explicit parentheses](https://en.wikipedia.org/wiki/S-expression), hence:

 - "3cm^3^" is well-defined by SI, but invites ambiguous interpretation. Better
   to be safe and add explicit parentheses.
 - "3(cm)^3^" is unambiguous. I would still try to avoid this, since it's using
   derived units (cubic centimetres). We could instead use 0.000003m^3^, or
   3×10^-6^m^3^, or 3μ(m^3^). The parentheses in the latter are a bit ugly, but
   aesthetics are less important than specificity, and it's arguable which of
   these forms looks nicer.
 - "3c(m^3^)" is also unambiguous (although a different quantity to the above).
   In this case there aren't many zeros, so we could just write 0.03m^3^.

Another way to reduce ambiguity *and* ugliness is to give explicit names to our
units, like calling the cubic metre a "kuub" (as mentioned above). These two
quantities could then be called 3 microkuub and 3 centikuub, respectively.

### Symbolising the Kuub ###

We can't symbolise kuub with a "k" or a "c", since those clash with "kilo" and
"centi". One obvious symbol would be a cube, which we can draw isometrically as
a 'hexagon with spokes'. The closest Unicode I can find for this is Ⓨ (the
letter Y inside a circle), so these quantities would be 3µⓎ and 3cⓎ.

Alternatively we could have a "power-agnostic" symbol, e.g. writing
3µ<span style="display: inline-block; transform: rotate(180deg);">𒑳</span> and
3c<span style="display: inline-block; transform: rotate(180deg);">𒑳</span>.
Here metres are written as
<span style="display: inline-block; transform: rotate(180deg);">𒀹</span> and
multiple arrows get "stacked". This allows inverse metres to be written as
<span style="display: inline-block; transform: scaleX(-1);">𒀹</span>, so up and
down arrows cancel out. We could also use the "forwards" (rightwards) direction
for time, writing seconds as
<span style="display: inline-block; transform: rotate(-90deg);">𒀹</span> and
inverse seconds as 𒀹. For example, the acceleration at Earth's surface g =
9.81<span style="display: inline-block; transform: rotate(180deg);">𒀹</span>𒃵.

## Problems with Time ##

Time is tricky. The base unit in SI is the second, but it's common for metric
to use hours, e.g. km/h or "kilometres per hour", where:

```
1 hour = 60 minutes
       = 60×60 seconds
       = 3600 seconds
```

One notable attempt at making time metric was the [decimal
time](https://en.wikipedia.org/wiki/Decimal_time) briefly adopted by France;
however, as I note in [the companion post](metric_red_herring.html),
the main advantages of metric don't come from using base 10. The [base
60](https://en.wikipedia.org/wiki/Sexagesimal) relationships between hours,
minutes and seconds are perfectly reasonable, and I would argue *preferable* to
base 10. The main problem is that we seem to have three units of time (seconds,
minutes and hours), when we would prefer a single base unit and some multipliers
(whether base 10 or otherwise).

In fact, the [current system of
hours/minutes/seconds](https://en.wikipedia.org/wiki/Minute#History) already
provides this! The hour is the base unit of time, and we define the minute as
one sixtieth of an hour: originally known as a "minuta" or "pars minuta prima"
("fraction", "small part" or "first small part"). Dividing by sixty again gives
a "pars minuta secunda" ("second small part", or "second"). This system goes on,
with "minuta tertia" ("thirds"), etc. The naming is nice and logical, akin to
the "billion", "trillion", "quadrillion", etc. used for large numbers. Despite
mostly being used for time, these multipliers are actually generic, since
they're also used for angles, with one degree containing
[60 arcminutes, each of which contains 60
arcseconds](https://en.wikipedia.org/wiki/Minute_and_second_of_arc).

The main problem with time is that SI uses the second as its base unit rather
than the hour, so attempting to change that would either require conversion
factors of 3600 all over the place, or require redefinition of almost every
other unit. I think it's easier to keep the other units as they are and have the
second as our base unit of time. In particular, speeds should be expressed in
(prefixed multiples of) metres per second, and so on.

The minute and hour should be replaced by sexagesimal multiples of the second
(for sexagesimal prefices see "Problems with Prefices" below). If we want to
avoid the historical baggage and numerical ambiguity of the name "second" we
could shorten it to "sec", which is already commonly understood. The plural
"secs" invokes the idea of sixes (akin to "sex", as in "sexagesimal"), which is
an interesting side effect.

## Problems with Angles ##

Since I've now mentioned angles, it's worth giving them some thought here.
Angles are dimensionless, so choosing a base angle is the same as choosing how
many divisions should go into a full turn. An obvious choice is
[one](https://en.wikipedia.org/wiki/Turn_(angle)), which lets us use units like
Hz for angular velocity.

SI makes a different choice: the [radian](https://en.wikipedia.org/wiki/Radian),
whose sine and cosine functions have slopes oscillating between ±1. Larger
divisions, like full turns, give a steeper slope; smaller divisions, like
degrees, give a shallower slope. Sine and cosine are derivatives of each other
(modulo a minus sign), scaled by this maximum slope; hence this slope acts as a
constant of proportionality (like those discussed in the section "Metric
Conversions Multiply By One" of [the companion
post](metric_red_herring.html)). Forcing this slope to be 1
gives us the radian as our unit of angle. (Radians can be defined in other ways,
e.g. the angle subtended by an arc whose length equals its radius; I just think
this sine/cosine relationship fits the 'constant of proportionality' template
nicely). Using radians as our unit for angles gives derived units like radians
per second (AKA rad/s) for angular velocity.

There are a little over six radians in a full turn; the exact ratio is
irrational, around 6.283.... This number is so ubiquitous in geometry, and
mathematics more broadly, that we denote it with the symbol τ; hence one turn
equals τ radians. If we use radians as our base angle, certain formulae become
quite simple (e.g. the length of an arc is the angle in radians multiplied by
the radius), but others require τ (e.g. angular frequency in Hertz is the
angular velocity in radians per second divided by τ). If we use turns as our
base angle, these sets of formulae switch around, e.g. converting turns to Hertz
requires no conversion factor, but the length of an arc is the angle in turns
multiplied by the radius multiplied by τ.

I'm undecided as to which is preferable as a base/default, since each have their
merits. Current practice is to use radians by default, and use a factor of τ
when talking about turns, e.g. "3τ" is three turns. This makes sense, but this
irrational multiplier makes many "everyday" situations more complicated than if
we use turns (e.g. we can get [surprisingly far without
irrationals](https://en.wikipedia.org/wiki/Rational_trigonometry)!). This, along
with the 1-to-1 conversion between units like the Hertz and Becquerel, makes me
favour turns as our unit of angle.

As for other approaches:

 - [Gradians](https://en.wikipedia.org/wiki/Gradian) should be avoided, since
   their definition as one four-hundredth of a turn is completely arbitrary and
   redundant. (Remember, metric isn't about powers of ten!)

 - The use of π to denote a half-turn is likewise redundant and
   [arbitrary](https://en.wikipedia.org/wiki/Pi#Adoption_of_the_symbol_%CF%80)

 - The division of a turn into 360 degrees gives many nice, round numbers for
   easing mental arithmetic; indicating that a generic
   [trecentosexagesimal](https://en.wikipedia.org/wiki/List_of_numeral_systems#Standard_positional_numeral_systems)
   prefix is a good idea. Repurposing the word "degree" for this would be easy
   for angles, as in "90 degree turn" for a quarter turn (AKA a "right angle");
   it would be less familiar in other contexts, like "2 degree Volts" for
   1/180th of a Volt; and it would unfortunately be ambiguous in others, like
   "3 degree Kelvin" for 1/120th of a Kelvin (*not* 3 Kelvin!)

 - The subsequent sexagesimal divisions of degrees into arcminutes (sixtieths of
   a degree turn) and arcseconds (sixtieths of an arcminute) are inconsistent
   and confusing, and hence should be avoided.

 - The subsequent decimal divisions into e.g. milliarcseconds are simply absurd.

## Problems with Units ##

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

## Problems with Prefices ##

10 isn't a particularly good base for a number system. It's only divisible by 1,
2, 5 and 10, which obscures many patterns (i.e. those which don't have period 2
or 5) and overly-complicates the representation of otherwise 'simple' numbers
(like the 1Mim example in [the companion
post](metric_red_herring.html)). The binary prefices ("kibi", "mibi",
etc.) are preferable in this regard, although their exponents (10, 20, ...) are
rather arbitrary; we only use them since they're close to more familiar powers
of 10.

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

I'm not aware of any sexagesimal prefices (powers of 60), but they would be
useful e.g. for abbreviating times to begin with, and maybe spreading their
highly composite nature into other areas. The existing system of "minutes",
"seconds", "thirds", etc. is useful for *dividing* by 60, although it would be
preferable to use it in prefix rather than postfix position for consistency
(e.g.  "minutearc" rather than "arcminute"). [Metric prefices traditionally used
Latin for smaller parts (e.g. "centi" and "milli") and Greek for larger parts
(e.g.  "kilo" and
"mega")](https://en.wikipedia.org/wiki/Metric_system#Prefixes_for_multiples_and_submultiples).
The minutes/seconds/thirds system likewise uses Latin for the smaller parts, so
we should use Greek for our sexagesimal multiples: "prota" would be sixties,
"defter" would be three thousand six hundreds, "trito" would be two hundred and
sixteen thousands, and so on.

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
Whilst I don't think we'll see speedometers denoting logarithmically-scaled
dozenal divisions of lightspeed any time soon, I think there are some definite
steps we can take to improve our use of units.

 - [Switching to metric](metric_red_herring.html) would give the
   biggest "bang for buck ratio", since it lets us forget about imperial
   nonsense. Most of the world is here already, although the UK needs to sort
   out its road network (but that's a whole other issue!).

 - Units which are lesser-used but logically-preferable can be used right now
   with minimal effort, e.g. temperatures in Kelvin, angles in turns or radians.

 - We can avoid the main inconsistencies of metric by avoiding the names "litre"
   and "kilogram". I'm partial to the names "millikuub" and "grave",
   respectively. This is quite low-effort, since the quantities would remain the
   same, only our terminology would change, so anyone can start doing this today
   (with a side note that these are foreign/archaic names for cubic metres and
   kilograms, as needed).

 - To fix temporal issues we can introduce new sexagesimal multipliers for
   seconds, e.g. instead of minutes we can have 1 protasec = 60 sec, and instead
   of hours we can have 1 deftersec = 60 protasec = 60×60 sec = 3600 sec. Again,
   since the quantities remain the same, we can start doing this right now;
   although there's no authoritative definition we can to point to for these, so
   it would be an uphill battle (maybe more suited to contrarian, non-conformist
   types).

 - If we don't mind numerical changes we can switch from base-10 prefices
   "kilo", "mega", etc. to base-2 "kibi", "mibi", etc. since they're close
   enough, and the base is more sensible. Thankfully these are standardised, and
   the meaning should be mostly clear: perhaps listeners would think we have a
   speech impediment, but they'd get the right order of magnitude. It should go
   without saying, but we should *not* do this when being misinterpreted would
   have bad consequences!

More long-term I think that [switching to dozenal would be
nice](https://en.wikipedia.org/wiki/Duodecimal#Advocacy_and_%22dozenalism%22),
and/or hexadecimal (helped by the ubiquity of binary computers), but attempts to
do so have struggled for decades. In particular, any advantages of teaching and
learning in these systems would be countered by the difficulty of applying them
in the "real world" with its prevalence of base 10.
