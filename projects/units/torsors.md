---
title: Torsors
---

*Torsors* are 'relative' values, where we can't add them but we can find their
difference. Cruicially, such 'differences' are *not* values of our torsor. For
example, points in space (of any dimension) cannot be meaningfully 'added'; but
we *can* find their difference; and that difference is a vector (not a point).

Furthermore, we can always add such 'differences' to our torsor values,
resulting in more torsor values. In particular, the following equation holds:

```{.unwrap pipe="sh | math block nosem"}
tag() {
  printf '<%s>' "$1"
  cat
  printf '</%s>' "$1"
}

row() { tag 'mrow'; }

mi() { echo "$1" | tag 'mi'; }
mo() { echo "$1" | tag 'mo'; }


D=$({
  mi 'tDiff'
  mo '⁡'
  { mo '('; mi 'y'; mo ','; mi 'x'; mo ')'; } | row
} | row)

{
  {
    mi 'tAdd'
    mo '⁡'
    { mo '('; mi 'x'; mo ','; echo "$D"; mo ')'; } | row
  } | row
  mo '='
  mi 'y'
} | row
```

In the case of points and vectors, adding the vector
`var 'y'; var 'x';`{.unwrap pipe="sh | mapply minus | math"} to the point
`x`{.unwrap pipe="var | math"} gives the point `y`{.unwrap pipe="var | math"}.

## Uses ##

Torsors are useful for avoiding arbitrary coordinates, i.e. when there's no
natural/obvious way to define 'zero'. We would like to build an approach to
geometry which avoids the need for arbitrary choices. Projective geometry is a
good starting point: we can certainly combine two points to yield the line
joining them; and dually we can combine lines to yield the point where they meet
(potentially 'at infinity'). However, it's not clear whether adding a line to a
point can 'undo' such combinations.

We can specialise a projective space to an affine space by choosing a
distinguished line (normally this is the line at infinity; but we can use any
finite line as a One place we can use this is the affine plane, where there is
no 'zero' point or line:

 - The 'difference' between two points is a *directed line segment*. Note that
   it is *not* a line, since there's no way to choose an orientation when taking
   a point from itself (line *segments* avoid this since such zero-length
   segments have *no* orientation!)
 - Adding a directed line segment to a point yields another point.

We can define line segments in projective space too; but there is no way to
uniquely 'transport' segments to coincide with arbitrary projective points.
Dually, for affine lines:

 - The 'difference' between two lines is a *directed angle* (where 'angle'
   refers to an intersection of lines; not any particular measure!)
 - Adding a directed angle to a line yields a line. NO! It requires a
   point on the line, for the intersection! Otherwise we have a *direction*!

Again, the second property is affine, so we can Notice that these segments have
no particular 'length' (or equivalent metric, like *quadrance*), since
projective geometry doesn't impose any. Likewise, these angles have no
'arc-length' or 'radius'. As a consequence, we cannot 'transport' these objects
through the space in a unique way: for example, vector addition is as easy as
'gluing' the start of one vector to the end of another; but this requires
'transporting' the vectors around (or, equivalently, redefining their
origin). In projective geometry we can 'transport' line segments in

(Directed) line segments can interact with (directed) angles: adding an angle to
a segment yields another segment: the empty angle acts as identity. I can't
think of another interaction between lines/segments and angles...

We can imagine rotating around a point, by adding an angle to a point. Adding a
line segment to a point doesn't quite represent translations, since the point is
irrelevant. Scaling can use a distinguished point (as the centre), but there is
no 'unit vector' in projective geometry, so it's unclear how much to scale by:
we need two line segments, to bring into coincidence, but doing so can also
introduce a rotation.

Two angles of a distinguished point point except there's no need for a
distinguished point in that case; they

## Affine ##

TODO
