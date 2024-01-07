---
title: Negative Digits
---

This page describes an elegant method for taking one number away from another,
which I claim is simpler than the traditional "long subtraction" (that I was
taught in school), and it requires no "carrying". It uses the idea of
[negative digits](https://en.wikipedia.org/wiki/Signed-digit_representation),
which also (in case you're interested) happens to be a nice generalisation of
the way we represent numbers!

## Notation ##

"Taking away" is often represented as *subtraction*, written with a *minus
sign*, like this (these are just arbitrary numbers, picked at random):

```{.unwrap pipe="sh | math block"}
{ num '102843'; num '67592'; } | mapply 'minus'
```

This is sometimes called a "binary minus sign", since it acts on *two*
numbers, written to its left and right.

I find [subtraction to be overly-complicated](subtraction.html), and instead
prefer to *add a negative*. That would usually be written like this:

```{pipe="sh > y.mml"}
num '67592' | neg
```

```{pipe="sh > add_negative.mml"}
{ num '102843'; cat y.mml; } | add
```

```{.unwrap pipe="sh | math minus block"}
cat add_negative.mml
```

This time we have a "unary minus sign", since it's only acting on *one* number,
written to its right (if you prefer, you can think of the unary minus sign as
acting like a binary minus sign whose left-hand-side is always
`0`{.unwrap pipe="num | math"}).

Mixing between these two different "minus signs" is awkward, so I prefer to
write negatives by giving them an over-bar, which looks like this:

```{.unwrap pipe="sh | math block"}
cat add_negative.mml
```

This is the notation I'll be using below. If you prefer, you can use subtraction
and/or minus signs instead; although you may need to space-out your writing to
keep things nicely aligned!

### Reading `cat y.mml`{.unwrap pipe="sh | math"} ###

We're familiar with how positive numbers are made from their positive digits,
by treating each digit as a multiple of the base raised to decreasing powers
(in decimal, the base is ten); for example:

```{.unwrap pipe="sh | math block"}
{
  num '142'
  {
    { num '1'; { num '10'; num '2'; } | mapply 'power'; } | mult
    { num '4'; { num '10'; num '1'; } | mapply 'power'; } | mult
    { num '2'; { num '10'; num '0'; } | mapply 'power'; } | mult
  } | add
  { num '100'; num '40'; num '2'; } | add
} | mapply 'eq'
```

In exactly the same way, we can interpret negative numbers as having *negative
digits*, e.g.

```{.unwrap pipe="sh | math block"}
{
  num 142 | neg
  {
    { num '1' | neg; { num '10'; num '2'; } | mapply 'power'; } | mult
    { num '4' | neg; { num '10'; num '1'; } | mapply 'power'; } | mult
    { num '2' | neg; { num '10'; num '0'; } | mapply 'power'; } | mult
  } | add
  {
    num '100' | neg
    num  '40' | neg
    num   '2' | neg
  } | add
} | mapply 'eq'
```

In other words, we can read a negative number like
`cat y.mml`{.unwrap pipe="sh | math"} in two ways: as the negative of
`67592`{.unwrap pipe="num | math"} (which itself is a sum of multiples of
`10`{.unwrap pipe="num | math"}), i.e.

```{.unwrap pipe="sh | math block"}
{
  cat y.mml
  { num '1' | neg; num '67592'; } | mult
  {
    num '1' | neg
    { num '60000'; num '7000'; num '500'; num '90'; num '2'; } | add
  } | mult
} | mapply 'eq'
```

*Alternatively* we can read `cat y.mml`{.unwrap pipe="sh | math"} as a sum of
*negative* multiples of `10`{.unwrap pipe="num | math"}, e.g.

```{.unwrap pipe="sh | math block"}
{
  cat y.mml
  {
    num '60000' | neg
    num  '7000' | neg
    num   '500' | neg
    num    '90' | neg
    num     '2' | neg
  } | add
} | mapply 'eq'
```

## Setup ##

We'll use the example above, and calculate
`cat add_negative.mml`{.unwrap pipe="sh | math"}. We'll start by writing one
number above the other, like in normal long-addition; since we're adding, it
doesn't matter which goes above/below. We align their digits to the right, so
the "ones" line up, the "tens" line up, etc. (I've also inserted a leading
`0`{.unwrap pipe="num | math"} as "padding"). As per tradition, we write
`<mo>+</mo>`{.unwrap pipe="math nosem"} on the bottom left and draw a horizontal
line underneath (our result will be written below this line):

<table>
<tr>
 <td></td>
 <td style="text-align: right;">`102843`{.unwrap pipe="num | math"}</td>
</tr>
<tr>
 <td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
 <td style="text-align: right;">`067592`{.unwrap pipe="num | neg | math"}</td>
</tr>
<tr><td colspan="2"><hr></td></tr>
</table>

## Calculating ##

We proceed just as in long-addition, adding the right-most digits (the "ones"),
and progressing to the left. Reading negative *numbers* as a sequence of
negative *digits* lets us do two things: firstly, we will apply the usual
digit-by-digit algorithm *regardless* of whether they're positive or negative;
and secondly we'll allow our answer to use both positive *and* negative digits
if needed.

Let's start with the right-most column:
`{ num '3'; num '2' | neg; } | add`{.unwrap pipe="sh | math"}. Their sum is
`1`{.unwrap pipe="num | math"}, so that's the right-most digit of our answer:

<table>
<tr>
 <td></td>
 <td style="text-align: right;">`102843`{.unwrap pipe="num | math"}</td>
</tr>
<tr>
 <td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
 <td style="text-align: right;">`067592`{.unwrap pipe="num | neg | math"}</td>
</tr>
<tr><td colspan="2"><hr></td></tr>
<tr><td></td><td style="text-align: right;">`1`{.unwrap pipe="num | math"}</td>
</tr>
</table>

Moving left one column, we need to calculate
`{ num '4'; num '9' | neg; } | add`{.unwrap pipe="sh | math"}, which equals
`5`{.unwrap pipe="num | neg | math"}, so that's the next digit of our answer.
Note that the usual long-subtraction algorithm would give up here, and ask us to
"carry a one" from the third column!

```{pipe="sh > 51.mml"}
echo '<mrow>'
num '5' | neg
num '1'
echo '</mrow>'
```

<table>
<tr>
 <td></td>
 <td style="text-align: right;">`102843`{.unwrap pipe="num | math"}</td>
</tr>
<tr>
 <td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
 <td style="text-align: right;">`067592`{.unwrap pipe="num | neg | math"}</td>
</tr>
<tr><td colspan="2"><hr></td></tr>
<tr><td></td><td style="text-align: right;">
`cat 51.mml`{.unwrap pipe="sh | math nosem"}
</td>
</tr>
</table>

```{pipe="sh > 85.mml"}
{
  { num '8'; num '5' | neg; } | add
  num '3'
} | mapply 'eq'
```

```{pipe="sh > 27.mml"}
{
  { num '2'; num '7' | neg; } | add
  num '5' | neg
} | mapply 'eq'
```

```{pipe="sh > 06.mml"}
{
  { num '0'; num '6' | neg; } | add
  num '6' | neg
} | mapply 'eq'
```

```{pipe="sh > 10.mml"}
{
  { num '1'; num '0' | neg; } | add
  num '1'
} | mapply 'eq'
```

Our result now contains positive and negative digits, but that's fine. We can
proceed through the rest: `cat 85.mml`{.unwrap pipe="sh | math"},
`cat 27.mml`{.unwrap pipe="sh | math"}, `cat 06.mml`{.unwrap pipe="sh | math"},
`cat 10.mml`{.unwrap pipe="sh | math"}. The overall result is:

```{pipe="sh > sum_result.mml"}
printf '<mrow>'
num '1'
num '6' | neg
num '5' | neg
num '3'
num '5' | neg
num '1'
printf '</mrow>'
```

<table>
<tr>
 <td></td>
 <td style="text-align: right;">`102843`{.unwrap pipe="num | math"}</td>
</tr>
<tr>
 <td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
 <td style="text-align: right;">`067592`{.unwrap pipe="num | neg | math"}</td>
</tr>
<tr><td colspan="2"><hr></td></tr>
<tr><td></td><td style="text-align: right;">
`cat sum_result.mml`{.unwrap pipe="sh | math nosem"}
</td>
</tr>
</table>

Note that we performed the above calculation by working right-to-left through
the digits, but that's not actually required: we can add up the columns in *any*
order, since they're all independent (unlike traditional long-subtraction, where
some columns need to carry/borrow from others).

## Normalising ##

Writing negative numbers with negative digits is fine, but our result has a
*mixture* of positive and negative digits. Such representations are redundant,
similar to writing a fraction like
`{ num '6'; num '8'; } | mapply 'divide'`{.unwrap pipe="sh | math"} (which
should be reduced to
`{ num '3'; num '4'; } | mapply 'divide'`{.unwrap pipe="sh | math"}). We
can likewise reduce a number which mixes positive and negative digits.

```{pipe="sh > mixed.mml"}
{
  cat sum_result.mml
  {
    num '100000'
    num  '60000' | neg
    num   '5000' | neg
    num    '300'
    num     '50' | neg
    num      '1'
  } | add
} | mapply 'eq'
```

When we think of such numbers as a sum, like
`cat mixed.mml`{.unwrap pipe="sh | math nosem"}, then the positive digits are
pushing up the total, and negative digits are pushing it down. Numbers with
mixed digits are pushing in both directions, which is the cause of redundancy:
for example, `{ num '10'; num '7' | neg; } | add`{.unwrap pipe="sh | math"} is
just an awkward way of writing `3`{.unwrap pipe="num | math"}, and
`{ num '30' | neg; num '2'; } | add`{.unwrap pipe="sh | math"} is just
`28`{.unwrap pipe="num | neg | math"}.

The less-significant digits (those to the right) cannot represent numbers larger
than the more-significant digits (to the left), so the overall sign of the
result will match that of the most-significant/left-most digit. In our example
that's `1`{.unwrap pipe="num | math"}, which is positive, so the whole number is
positive; and hence we can reduce it to using only positive digits. Thankfully
we can split up the problem, replacing one pair of neighbouring digits that have
opposite signs, and repeating until they all have the same sign.

```{pipe="sh > first.mml"}
printf '<mrow>%s%s</mrow>' "$(num '1')" "$(num '6' | neg)"
```

```{pipe="sh > 45.mml"}
{
  printf '<mrow>%s%s</mrow>' "$(num '4')" "$(num '5' | neg)"
  { num '40'; num '5' | neg; } | add
  num '35'
} | mapply 'eq'
```

```{pipe="sh > 35.mml"}
{
  printf '<mrow>%s%s</mrow>' "$(num '3')" "$(num '5' | neg)"
  { num '30'; num '5' | neg; } | add
  num '25'
} | mapply 'eq'
```

```{pipe="sh > first_two.mml"}
{
  cat first.mml;
  { num '10'; num '6' | neg;} | add;
} | mapply 'eq'
```

```{pipe="sh > result_stepped.mml"}
printf '<mrow>'
num '4'
num '5' | neg
num '3'
num '5' | neg
num '1'
printf '</mrow>'
```

```{pipe="sh > step2.mml"}
printf '<mrow>'
num '3'
num '5'
num '3'
num '5' | neg
num '1'
printf '</mrow>'
```

We can start with the first two digits,
`cat first.mml`{.unwrap pipe="sh | math"}: technically these represent
`num '100000'; num '60000' | neg;`{.unwrap pipe="sh | add | math"}, but we
can ignore the common factor of `10000`{.unwrap pipe="num | math"} and treat
them like the two-digit number
`cat first_two.mml`{.unwrap pipe="sh | math"}. This is just a convoluted way of
writing the number `04`{.unwrap pipe="num | math"}, so we can replace those two
digits and write our total as
`cat result_stepped.mml`{.unwrap pipe="sh | math"}. This still has a mixture of
digits, so we can replace the mis-matched pair
`cat 45.mml`{.unwrap pipe="sh | math"} to get
`cat step2.mml`{.unwrap pipe="sh | math"}; then
`cat 35.mml`{.unwrap pipe="sh | math"} to get
`35251`{.unwrap pipe="num | math"} which no longer contains mixed digits, and is
hence fully reduced.

Note that this reduction has not changed the overall number we calculated: all
of the above steps are just different ways to write the same thing. We can check
this by adding each of them to `67592`{.unwrap pipe="num | math"} to ensure that
we get `102843`{.unwrap pipe="num | math"} every time.

First, we can see that the original mixed-digit answer does indeed work:

<table>
<tr><td></td><td style="text-align: right;">
`cat sum_result.mml`{.unwrap pipe="sh | math"}
</td></tr>
<tr>
<td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
<td style="text-align: right;">`67592`{.unwrap pipe="num | math"}</td>
</tr>
<tr><td colspan="2"><hr></td></tr></tr>
<tr><td></td><td style="text-align: right;">
`102843`{.unwrap pipe="num | math"}
</td>
</tr>
</table>

The first step of our reduction is also valid:

<table>
<tr><td></td><td style="text-align: right;">
`cat result_stepped.mml`{.unwrap pipe="sh | math"}
</td></tr>
<tr>
<td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
<td style="text-align: right;">`67592`{.unwrap pipe="num | math"}</td>
</tr>
<tr><td colspan="2"><hr></td></tr></tr>
<tr><td></td><td style="text-align: right;">
`102843`{.unwrap pipe="num | math"}
</td></tr>
</table>

The next step of the reduction also works (although the check now involves a
carry):

<table>
<tr><td></td><td style="text-align: right;">
`cat step2.mml`{.unwrap pipe="sh | math"}
</td></tr>
<tr>
<td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
<td style="text-align: right;">`67592`{.unwrap pipe="num | math"}</td>
</tr>
<tr><td colspan="2"><hr></td></tr></tr>
<tr><td></td><td style="text-align: right;">
`92843`{.unwrap pipe="num | math"}
</td></tr>
<tr><td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
<td style="text-align: right;">`10000`{.unwrap pipe="num | math"}</td></tr>
<tr><td colspan="2"><hr></td></tr>
<tr><td></td><td style="text-align: right;">
`102843`{.unwrap pipe="num | math"}
</td></tr>
</table>

Finally we can show that the fully-reduced form, with all-positive digits, is
indeed the correct answer (this time requiring two carries!):

<table>
<tr><td></td><td style="text-align: right;">
`35251`{.unwrap pipe="num | math"}
</td></tr>
<tr>
<td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
<td style="text-align: right;">`67592`{.unwrap pipe="num | math"}</td>
</tr>
<tr><td colspan="2"><hr></td></tr></tr>
<tr><td></td><td style="text-align: right;">
`92743`{.unwrap pipe="num | math"}
</td></tr>
<tr><td>`<mo>+</mo>`{.unwrap pipe="math nosem"}</td>
<td style="text-align: right;">`10100`{.unwrap pipe="num | math"}</td></tr>
<tr><td colspan="2"><hr></td></tr>
<tr><td></td><td style="text-align: right;">
`102843`{.unwrap pipe="num | math"}
</td></tr>
</table>
