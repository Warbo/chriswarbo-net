centimetres/kilonewtons/millilitres/etc. are a red herring: there are
100centifeet in a foot and 1000 pounds in a kilopound. "centi", "milli",
"femto", "kilo", "giga", etc. are just generic ways to abbreviate large
numbers. There's an equivalent system for base 2, where "kibi" is 2^10 (~1000),
"mibi" is 2^20 (~1000000), etc.

The actual advantages of metric are:
 - There is a single unit for each dimension, e.g. all distances are in metres
   (again, "5cm" means "5 hundreths of a metre", so it's talking about metres);
   all forces are in newtons. This is preferable to the mish-mash of imperial
   units: whilst everyday usage seems to have (thankfully) given up on
   barleycorns and furlongs, it's still excruciating to use
   miles/yards/feet/inches, or stones/pounds/ounces, or
   gallons/pints/fluid-ounces.
 - The scaling factor when converting between dimensions is defined as 1. For
   example accelerating 1kg of mass by 1m/s^2 takes 1N of force (i.e. we can
   plug numbers straight into Newton's law F=ma without needing to remember any
   conversion constants; we can't do that with e.g. stones, inches and
   slugs). Likewise if we exert that 1N of force over a distance of 1m, it will
   take 1J of energy (again, we would need some arbitrary multiplier to do this
   with ounces, yards and calories). If that energy were released over 1 second
   it would require a power of 1W. If we supply that via a 1V electric motor it
   would need a current of 1A. And so on.


My only problems with metric are:
 - The litre is a stupid unit. It's 1 milli-cubic-metre. We should use a
   catchier name for the cubic metre, like stere or kuub, and label liquids with
   milli- and micro- amounts of that.
 - Choosing the kilogram as the standard unit of mass (e.g. in the equations I
   gave above), rather than the gram is stupid. The
   kilo/centi/mega/etc. prefixes still work, but we're effectively introducing
   1000 or 1/1000 into the conversion factors. We should use a different name
   for the kilogram, which doesn't clash with the prefix system.

Of course, it would be nice to define measurements in terms of natural units
like the speed of light and planck's constant, but they're so far removed from
human scales that we would need abbreviations or synonyms to avoid the huge
exponents. If we ever switch to a different units system, it should absolutely
use those constants as the base units, and either base-2 or base-12 prefixes.
