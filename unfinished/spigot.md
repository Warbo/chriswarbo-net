Use the `spigot` package in Nixpkgs, and investigate whether its
continued-fraction output gets stuck adding 0.111... to 0.000...

In principle it should be able to return approximations like 1/1,
2/2, 4/4, etc. since their "neighbours" (2/1 & 0/1, 1/2 & 3/2, 3/4 & 5/4, etc.)
are known to be further away.

According to the manual, spigot uses rational intervals to bound the values it's
calculating. This can cause a problem with place-value output (is the above
0.something or 1.something?), and I think it causes a problem with
continued-fraction representations too; e.g. they can't subtract fractions, so
they'll put-off crossing the 0 -> 1 threshold until being sure (i.e. when at
least one of the strings of zeros or ones ends). That makes continued fractions
a poor choice for internal representation, even if they lead to the "best"
rational approximations when *not* encountering such problems.
