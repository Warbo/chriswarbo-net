---
title: Negatives
packages: [ 'mathml' ]
---

<small>Part of [my units pages](/projects/units)</small>

"Negatives" (like [negative
numbers](https://en.wikipedia.org/wiki/Negative_number)) are a great idea in
many situations, but they can also be tricky. Here are some suggestions for
making them easier to work with, and hopefully easier to understand.

NOTE: I will be using
["over-bar" notation for negation](negative_bar_notation.html) (like
`5`{.unwrap pipe="num | neg | math"})

## You Ain't Gonna Need It! ##

Whilst this may seem obvious, we should avoid unnecessary complications where
possible. Situations which are easily modelled without negative numbers
shouldn't have them shoe-horned in.

One case where this happens often is in computer programming, where many
programming languages have no way to represent
[natural numbers](https://en.wikipedia.org/wiki/Natural_number); and even those
which do can use confusing terminology like
["unsigned integer"](https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Signed-and-Unsigned-Types.html).
Values like `length`, `size`, etc. are often represented with "signed" types,
which forces all of the code using them to handle negative values, which make no
sense and should (hopefully) never occur!

## Defining Negatives with Plus and Zero ##

We can define the "meaning" of negatives using the following property:

```{pipe="sh > negative.mml"}
{
  {
    var 'x'
    var 'x' | neg
  } | add
  num '0'
} | mapply 'equivalent'
```

(@negative) `cat negative.mml`{.unwrap pipe="sh | math"}

In other words, the negative of something is whatever we can *add* to that thing
to get a result of `0`{.unwrap pipe="num | math"}.

Note that this definition *does not* involve subtraction, since [I consider
subtraction to be more trouble than it's worth](subtraction.html).

### The Negative of `0`{.unwrap pipe="num | math"} ###

We know that adding `0`{.unwrap pipe="num | math"} to anything will leave that
thing unchanged (`0`{.unwrap pipe="num | math"} is called the "identity" value
for addition), i.e.

```{pipe="sh > zero.mml"}
{
  { var 'x'; num '0'; } | add
  var 'x'
} | mapply 'eq'
```

(@zero) `cat zero.mml`{.unwrap pipe="sh | math"}

```{pipe="sh > neg_zero.mml"}
{
  { num '0'; num '0'; } | add
  num '0'
} | mapply 'eq'
```

This tells us that, as a special case,
`cat neg_zero.mml`{.unwrap pipe="sh | math"} (in the case that
`var 'x'; num '0';`{.unwrap pipe="sh | mapply eq | math"}).
Since that sum equals `0`{.unwrap pipe="num | math"}, it fits our above
definition of a negative. Hence the negative of
`0`{.unwrap pipe="num | math"} is itself `0`{.unwrap pipe="num | math"}:

(@zero-bar) `num '0' | neg; num '0';`{.unwrap pipe="sh | mapply eq | math"}

### "Double Negatives" (Negatives of Negatives) ###

If we take the negative of something that's already a negative, we get back the
original thing:

```{pipe="sh > double_neg.mml"}
{
  var 'x' | neg | neg
  var 'x'
} | mapply eq
```

(@double-negative) `cat double_neg.mml`{.unwrap pipe="sh | math"}

<details class="odd">
 <summary>Proof</summary>

We can add `0`{.unwrap pipe="num | math"} to a "double negative" without
changing it (as per equation (@zero)):

```{.unwrap pipe="sh | math block"}
{
var 'x' | neg | neg
{
  var 'x' | neg | neg
  num '0'
} | add
} | mapply 'eq'
```

We can replace `0`{.unwrap pipe="num | math"} with the sum of a value and its
negative (as per equation (@negative)). Let's replace the
`0`{.unwrap pipe="num | math"} with the sum of `x`{.unwrap pipe="var | math"}
and its negative `x`{.unwrap pipe="var | neg | math"}:

```{.unwrap pipe="sh | math block"}
{
  var 'x' | neg | neg
  {
    var 'x' | neg | neg
    var 'x'
    var 'x' | neg
  } | add
} | mapply 'eq'
```

Let's rearrange this sum, and introduce parentheses to make the next step
clearer:

```{.unwrap pipe="sh | math block"}
{
  var 'x' | neg | neg
  {
    var 'x'
    printf '<mrow><mo>(</mo>%s<mo>)</mo></mrow>' \
           "$({ var 'x' | neg; var 'x' | neg | neg; } | add)"
  } | add
} | mapply 'eq'
```

The expression in the parentheses is the sum of a value
(`x`{.unwrap pipe="var | neg | math"}) and its negative
(`x`{.unwrap pipe="var | neg | neg | math"}), which is
`0`{.unwrap pipe="num | math"} (according to equation (@negative)):

```{.unwrap pipe="sh | math block"}
{
  var 'x' | neg | neg
  { var 'x'; num '0'; } | add
} | mapply 'eq'
```

Finally, we can get rid of the addition of zero since it doesn't change the
value (as per equation (@zero)):

```{.unwrap pipe="sh | math block"}
{
  var 'x' | neg | neg
  var 'x'
} | mapply 'eq'
```

QED

</details>

Hence such "double negatives" will "cancel out".

### Expanding, Contracting and Shifting Negatives ###

When negatives are multiplied, we can "expand" the bar to cover the whole
multiplication:

```{pipe="sh > expand.mml"}
{
  { var 'x' | neg; var 'y'; } | mult
  { var 'x'; var 'y'; } | mult | neg
} | mapply 'eq'
```

(@expand) `cat expand.mml`{.unwrap pipe="sh | math"}

<details class="odd">
  <summary>Proof</summary>

Adding `0`{.unwrap pipe="num | math"} to our multiplication won't change its
value (equation (@zero)):

```{.unwrap pipe="sh | math block"}
{
  { var 'x' | neg; var 'y'; } | mult
  {
    { var 'x' | neg; var 'y'; } | mult
    num '0'
  } | add
} | mapply 'eq'
```

We can replace that `0`{.unwrap pipe="num | math"} with the sum of a value
(`var 'x'; var 'y';`{.unwrap pipe="sh | mult | math"}) and its negative
(`var 'x'; var 'y';`{.unwrap pipe="sh | mult | neg | math"}) as per equation
(@negative):

```{.unwrap pipe="sh | math block"}
{
  { var 'x' | neg; var 'y'; } | mult
  {
    { var 'x' | neg; var 'y'; } | mult
    { var 'x';       var 'y'; } | mult
    { var 'x';       var 'y'; } | mult | neg
  } | add
} | mapply 'eq'
```

The first two terms have a common factor of `y`{.unwrap pipe="var | math"},
which we can factor out:

```{.unwrap pipe="sh | math block"}
{
  { var 'x' | neg; var 'y'; } | mult
  {
    {
      { var 'x' | neg; var 'x'; } | add
      var 'y'
    } | mult
    { var 'x'; var 'y'; } | mult | neg
  } | add
} | mapply 'eq'
```

Those parentheses contain the sum of a value (`x`{.unwrap pipe="var | math"})
and its negation (`x`{.unwrap pipe="var | neg | math"}), which is equal to
`0`{.unwrap pipe="num | math"} (equation (@negative)):

```{.unwrap pipe="sh | math block"}
{
  { var 'x' | neg; var 'y'; } | mult
  {
    { num '0'; var 'y'; } | mult
    { var 'x'; var 'y'; } | mult | neg
  } | add
} | mapply 'eq'
```

Multiplying by `0`{.unwrap pipe="num | math"} always gives
`0`{.unwrap pipe="num | math"}:

```{.unwrap pipe="sh | math block"}
{
  { var 'x' | neg; var 'y'; } | mult
  {
    num '0'
    { var 'x'; var 'y'; } | mult | neg
  } | add
} | mapply 'eq'
```

Finally we can remove the addition (as per equation (@zero)):

```{.unwrap pipe="sh | math block"}
{
  { var 'x' | neg; var 'y'; } | mult
  { var 'x'; var 'y'; } | mult | neg
} | mapply 'eq'
```

QED

</details>

We can use the same approach for the second value (except we'll get a common
factor of `x`{.unwrap pipe="var | math"} in the proof), to show that:

```{pipe="sh > expand-right.mml"}
{
  { var 'x'; var 'y' | neg; } | mult
  { var 'x'; var 'y'; } | mult | neg
} | mapply 'eq'
```

(@expand-right) `cat expand-right.mml`{.unwrap pipe="sh | math"}

These equations also tell us we can "contract" a bar from an entire
multiplication down to just one of its factors.

In combination, these let us "shift" bars from one factor to another:

```{pipe="sh > shift.mml"}
{
  { var 'x' | neg; var 'y'; } | mult
  { var 'x'; var 'y' | neg; } | mult
} | mapply 'eq'
```

(@shift) `cat shift.mml`{.unwrap pipe="sh | math"}

### Multiplying Negatives ###

Consider the multiplication of *two* negatives: we can "expand" one of the bars
to cover the whole product (as per equation (@expand)):

```{.unwrap pipe="sh | math block"}
{
  printf '<mrow>%s<mo>×</mo>%s</mrow>' \
         "$(var 'x' | neg)" \
         "$(var 'y' | neg)"
  { var 'x'; var 'y' | neg; } | mult | neg
} | mapply 'eq'
```

From equation (@expand-right), we can replace
`var 'x'; var 'y' | neg;`{.unwrap pipe="sh | mult | math"} with
`var 'x'; var 'y';`{.unwrap pipe="sh | mult | neg | math"}:

```{.unwrap pipe="sh | math block"}
{
  printf '<mrow>%s<mo>×</mo>%s</mrow>' \
         "$(var 'x' | neg)" \
         "$(var 'y' | neg)"
  { var 'x'; var 'y'; } | mult | neg | neg
} | mapply 'eq'
```

This is a double negative, which cancels out (via equation (@double-negative)):

```{.unwrap pipe="sh | math block"}
{
  printf '<mrow>%s<mo>×</mo>%s</mrow>' \
         "$(var 'x' | neg)" \
         "$(var 'y' | neg)"
  { var 'x'; var 'y'; } | mult
} | mapply 'eq'
```

In this way, multiplications can always be simplified to contain either one
negative, or no negatives at all.

### The Role of `1`{.unwrap pipe="num | neg | math"} ###

We can always multiply values by `1`{.unwrap pipe="num | math"} without changing
them, i.e.
`var 'x'; { num '1'; var 'x'; } | mult;`{.unwrap pipe="sh | mapply eq | math"}
(we say `1`{.unwrap pipe="num | math"} is the "identity" value for
multiplication). This combines nicely with the expansion, contraction and
shifting of negatives:

```{.unwrap pipe="sh | math block"}
{
  var 'x' | neg
  { num '1';       var 'x'; } | mult | neg
  { num '1' | neg; var 'x'; } | mult
} | mapply 'eq'
```

This lets us think about a value's "direction" (`1`{.unwrap pipe="num | math"}
or `1`{.unwrap pipe="num | neg | math"}) separately to its "size"; for example,
whether a number is pointing "up" or "down" the number line.

### Normal (Canonical) Forms ###

Having multiple ways to write the same thing can be convenient during a
calculation, but we'd rather have a *single* way to write our "final result"; so
it can be easily compared to other results (by just comparing the symbols).

Such notation is called a "normal form" (or "canonical form"), and may be
familiar from dealing with fractions, where e.g.
`num '27'; num '54';`{.unwrap pipe="sh | mapply divide | math"} and
`num '3'; num '6';`{.unwrap pipe="sh | mapply divide | math"} *appear* to be
different numbers. We can write these in normal form by removing common factors,
to get `num '1'; num '2';`{.unwrap pipe="sh | mapply divide | math"} and
`num '1'; num '2';`{.unwrap pipe="sh | mapply divide | math"}; which are
obviously identical.

In the case of products, we can introduce and eliminate factors of
`1`{.unwrap pipe="num | math"}, and we can expand, contract and shift around
negatives between factors. Hence there are many ways to write down the same
value, and it would be nice to have a normal form for any "final results". The
most natural approach is the following:

 - Eliminate any factors of `1`{.unwrap pipe="num | math"}. This makes the
   expression as small as possible.
 - Expand all negatives to cover the whole expression, since this avoids having
   to choose between factors arbitrarily.
 - Eliminate any "double negatives", to get only zero or one negative. This is
   easy once all the negatives have been expanded.

For example, the following expression:

```{.unwrap pipe="sh | math block"}
{
  {
    num '3'
    var 'x' | neg
    { var 'y' | neg; var 'z'; } | add
  } | mult
  { var 'x'; var 'y' | neg; } | mult | neg
} | add
```

Would have the following normal form:

```{.unwrap pipe="sh | math block"}
{
  {
    num '3'
    var 'x'
    { var 'y' | neg; var 'z'; } | add
  } | mult | neg
  { var 'x'; var 'y'; } | mult
} | add
```

Note that the `var 'y' | neg; var 'z';`{.unwrap pipe="sh | add | math"} isn't
itself a product, so its negative can't expand or shift around. We could also
"multiply out" and factorise, but that's more about "sums and products" rather
than negatives, so I'll leave it up to taste, e.g.

```{.unwrap pipe="sh | math block"}
{
  var 'x'
  {
    { num '4'; var 'y'; } | mult
    { num '3'; var 'z'; } | mult | neg
  } | add
} | mult
```
