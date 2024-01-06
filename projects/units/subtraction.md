---
title: Problems with Subtraction
---

Subtraction is the operation of "taking away", and acts like the "opposite" of
addition. For example, we can use a subtraction to "undo" an addition:

```{.unwrap pipe="sh | math block minus"}
{
  {
    { var 'x'; var 'y'; } | add
    var 'y'
  } | mapply 'minus'
  var 'x'
} | mapply 'eq'
```

This is a fine idea, but unfortunately subtraction is not as well-behaved as
addition, and I personally think it causes more problems than it solves.

## Subtraction is *redundant* ##

We can define subtraction using addition and negation:

```{.unwrap pipe="sh | math block"}
{
  { var 'x'; var 'y';       } | mapply 'minus'
  { var 'x'; var 'y' | neg; } | add
} | mapply 'equivalent'
```

```{pipe="sh > neg_answer.mml"}
{
  { num '5'; num '7'; } | mapply 'minus'
  num '2' | neg
} | mapply 'eq'
```

Hence situations involving negatives don't need subtraction: we can stick to
additions, and sprinkle in some negatives as needed. Furthermore, if a situation
*doesn't* allow negatives, then it doesn't allow subtraction either, since
subtraction gives negative answers when the second value is larger than the
first (e.g. `cat neg_answer.mml`{.unwrap pipe="sh | math"}).

So any time we might want subtraction, we could add negatives instead; and any
time we can't use negatives, we also can't use subtraction. Thus subtraction
isn't *needed*. We might find it *useful*, but it's ultimately just an
abbreviation for adding negatives.

## Subtraction is *hard* ##

We've seen that subtraction is the same as adding negatives, but that doesn't
tell us whether one is "better" than the other. Now I'll show that adding is
preferable, since it follows many nice rules/patterns that subtraction doesn't;
hence making it easier to add negatives (both mentally, and on a computer)!

### Subtraction doesn't commute ###

```{pipe="sh > add_comm.mml"}
{
  { var 'x'; var 'y'; } | add
  { var 'y'; var 'x'; } | add
} | mapply 'eq'
```

```{pipe="sh > not_comm.mml"}
{
  { var 'x'; var 'y'; } | mapply 'minus'
  { var 'y'; var 'x'; } | mapply 'minus'
} | mapply 'neq'
```

```{pipe="sh > anti_comm.mml"}
{
  { var 'x'; var 'y'; } | mapply 'minus'
  { var 'y'; var 'x'; } | mapply 'minus' | neg
} | mapply 'eq'
```

We can swap around the values of an addition without changing the result: we say
addition *commutes*, i.e. that `cat add_comm.mml`{.unwrap pipe="sh | math"}.

In contrast, we *can't* swap around the values of a subtraction, since
`cat not_comm.mml`{.unwrap pipe="sh | math"}. In fact, doing so will negate the
result: `cat anti_comm.mml`{.unwrap pipe="sh | math"} (we say that subtraction
"anti-commutes").

### Subtraction doesn't associate ###

```{pipe="cat > add_left.mml"}
<mrow>
  <mrow>
    <mo>(</mo>
    <mi>x</mi>
    <mo>+</mo>
    <mi>y</mi>
    <mo>)</mo>
  </mrow>
  <mo>+</mo>
  <mi>z</mi>
</mrow>
```

```{pipe="cat > add_right.mml"}
<mrow>
  <mi>x</mi>
  <mo>+</mo>
  <mrow>
    <mo>(</mo>
    <mi>y</mi>
    <mo>+</mo>
    <mi>z</mi>
    <mo>)</mo>
  </mrow>
</mrow>
```

```{pipe="sh > add_flat.mml"}
{ var 'x'; var 'y'; var 'z'; } | add
```

Addition extends to arbitrarily-many inputs, which we call being "associative".
In particular, `cat add_left.mml`{.unwrap pipe="sh | math nosem"} is the same as
`cat add_right.mml`{.unwrap pipe="sh | math nosem"}, so we can just avoid
nesting altogether and write `cat add_flat.mml`{.unwrap pipe="sh | math"} (the
same goes when dealing with more than three inputs, nested in any way).

```{pipe="sh > sub_neq.mml"}
{
  {
    { var 'x'; var 'y'; } | mapply 'minus'
    var 'z'
  } | mapply 'minus'
  {
    var 'x'
    { var 'y'; var 'z'; } | mapply 'minus'
  } | mapply 'minus'
} | mapply 'neq'
```

```{pipe="sh > sub_left_example.mml"}
{
  {
    { num '1'; num '2'; } | mapply 'minus'
    num '3'
  } | mapply 'minus'
  {
    num '1' | neg
    num '3'
  } | mapply 'minus'
  num '4' | neg
} | mapply 'eq'
```

```{pipe="sh > sub_right_example.mml"}
{
  {
    num '1'
    { num '2'; num '3'; } | mapply 'minus'
  } | mapply 'minus'
  {
    num '1'
    num '1' | neg
  } | mapply 'minus'
  num '2'
} | mapply 'eq'
```

```{pipe="cat > sub_flat_example.mml"}
<mrow>
  <mn>1</mn>
  <mo>-</mo>
  <mn>2</mn>
  <mo>-</mo>
  <mn>3</mn>
</mrow>
```

In contrast, using subtraction with more than two values is ambiguous. For
example `cat sub_flat_example.mml`{.unwrap pipe="sh | math nosem"}
could mean `cat sub_left_example.mml`{.unwrap pipe="sh | math"} *or* it could
mean `cat sub_right_example.mml`{.unwrap pipe="sh | math"}, which give different
results. In general `cat sub_neq.mml`{.unwrap pipe="sh | math"}, so we're forced
to keep careful track of nesting when using subtraction.

```{pipe="sh > add_example_flat.mml"}
{ var 'a'; var 'b'; var 'c'; var 'd'; var 'e'; var 'f'; } | add
```

```{pipe="cat > add_example_nest.mml"}
<mrow>
  <mrow>
    <mo>(</mo><mi>a</mi><mo>+</mo><mi>b</mi><mo>)</mo>
  </mrow>
  <mo>+</mo>
  <mrow>
    <mo>(</mo><mi>c</mi><mo>+</mo><mi>d</mi><mo>)</mo>
  </mrow>
  <mo>+</mo>
  <mrow>
    <mo>(</mo><mi>e</mi><mo>+</mo><mi>f</mi><mo>)</mo>
  </mrow>
</mrow>
```

Associative operations are also better for computers, since they're trivial to
calculate in parallel. For example, if we have three processors, we can
calculate `cat add_example_flat.mml`{.unwrap pipe="sh | math"} in parallel by
transforming it into `cat add_example_nest.mml`{.unwrap pipe="sh | math nosem"};
the result will always be identical, but the nested parts can be calculated
independently, and hence on separate processors at the same time. We cannot
perform such transformations for operations that *aren't* associative; hence
subtraction can't easily be done in parallel.
