---
title: Extents (work in progress)
---

<small>Part of [my units pages](/projects/units)</small>

TODO: This is closely related to the idea of [Torsors](torsors.html), and the
difference between affine points being vectors.

 - This is the idea of a bounded *region*
 - It can be defined by its boundary, e.g. start and end points in 1D
 - It is distinct from an *interval* in two ways:
    - It is *relative*, e.g. more like a 1D vector
    - It is *signed*, again more like a 1D vector
 - Useful to distinguish position (e.g. on a number line) from
   difference-in-position
 - Good precursr for vectors:
    - In 1D, vectors *are* extents
    - In higher-dimensions, vectors also have a *direction* (maybe relatable to
      projective points-at-infinity)
 - Higher-dimensional extents include signed area, signed volume, etc.
 - In the 1D case it gives an unambiguous notion of subtraction for positive
   spaces like Nat: the difference between two Nats is an extent, not a Nat
    - This removes the difficulty of e.g. 1 - 2 = -2: the sign of the result
      tells us which direction to move, not 'where we end up'
    - We still get problems if we try asking which Nat is some extent away from
      another, e.g. taking the difference between 10 and 3 (-7) and asking what
      is an equivalent difference away from 2 (there is no -5 in Nat); yet this
      feels like a clearer case of 'not making sense'

Numbers and geometry are foundational to mathematics, and how we explain and
understand various phenomena. We can relate these concepts in two important, but
distinct, ways: as *positions* or as *extents*.

## Position versus extent ##

Consider the useful picture of a *number line*:

```
┌──┬──┬──┬──┬──┬──┬─⋯
0  1  2  3  4  5  6
```

This shows numbers as *positions* quite directly: the number four 'is' the
position (or *point*) labelled `4`.

The *extents* in this picture are a bit more abstract: we can find them by
'cutting' the line at the relevant label; e.g. cutting at the label `4` gives
the following:

```
┌──┬──┬──┬──┐
0  1  2  3  4
```

Whilst this is a perfectly good *length*, the idea of 'extent' that I'm after
needs two more things...

## Extents are relative ##

We will consider *the line* to be our extent, not the points or labels. Hence
all of the following are *the same* extent:

```
┌──┬──┬──┬──┐
0  1  2  3  4

┌──┬──┬──┬──┐
1  2  3  4  5

┌──┬──┬──┬──┐
32 33 34 35 36
```

Hence we can drop the labels, to get a line like `┌──┬──┬──┬──┐`
