---
title: Coverage-based data generation
---


TODO:

See whether coverage information from HPC can be used to guide data generation
for property checking.

Simplest to use falsify, since that has reified "choices" (attempting the same
with QuickCheck would require hard-coding responses to the `next` and `split`
functions of the generator; which is basically what `falsify` gives us!)

The awkward part is coming up with "actions" which manipulate these trees...
TODO: Investigate tree-editin
