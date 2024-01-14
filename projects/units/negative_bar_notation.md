---
title: Overbar notation for negation
packages: [ 'mathml' ]
---

There are many ways we can write down [negatives](negatives.html) (or more
general "negations"). The most common way is to prefix terms with a "minus
sign", like `5`{.unwrap pipe="num | neg | math minus"} for the negative of
`5`{.unwrap pipe="num | math"}, but I think [minus signs are too cumbersome and
confusing](minus.html). On this page, I'll explain why I instead prefer to draw
a line/bar *along the top* of a term (which I pronounce as "negative", rather
than "minus" or "bar").

In fact, any notation which affects the *entire length* of a term will have
these advantages; for example we could write in a different colour. I just find
overbars the most convenient (e.g. they don't require swapping pens)!

```{pipe="add > sum.mml"}
<ci>p</ci><cn>43</cn>
```

Here are some examples of this notation:

 - The negative of the number `5`{.unwrap pipe="num | math"} can be written
   `5`{.unwrap pipe="num | neg | math"}
 - The negative of a variable `x`{.unwrap pipe="var | math"} can be written
   `x`{.unwrap pipe="var | neg | math"}
 - The negative of a sum `cat sum.mml`{.unwrap pipe="sh | math"} can be written
   `cat sum.mml`{.unwrap pipe="sh | neg | math"}

Note that this [bar notation](https://en.wikipedia.org/wiki/Overline#Math_and_science)
is not original. Negatives are an
["additive inverse"](https://en.wikipedia.org/wiki/Additive_inverse), and it is
quite common to indicate other sorts of "inverse" with an overbar, for example:

 - [Negative digits](negative_digits.html), when writing numbers in a [signed or
   balanced base](https://en.wikipedia.org/wiki/Signed-digit_representation).
 - Negation of a logical or boolean expression.
 - Conjugation of a (hyper)complex number (negating its "imaginary" parts)
 - The complement (opposite/negation) of a set

## Readability and aesthetics: long and short bars ##

Consider a long expression like this:

```{pipe="sh > inner.mml"}
{
  num '123.45'
  {
    {
      num '42'
      { var 'y'; num '2'; } | mapply 'power'
    } | mult
    num '7'
  } | mapply 'divide'
} | add
```

```{.unwrap pipe="sh | tee long.mml | math block"}
{
  var 'x'
  {
    num '2'
    cat inner.mml | neg
    num '96'
  } | add
} | mult
```

The bar clearly shows which parts are negative, without needing more
parentheses; although we can add them if desired (in fact, [grouping used to be
written with such overlines](https://en.wikipedia.org/wiki/Vinculum_(symbol));
before parentheses became common!).

For comparison, using minus signs would give this:

```{.unwrap pipe="sh | math block minus"}
cat long.mml
```

Even if we don't like such long bars, we could instead multiply by
`1`{.unwrap pipe="num | neg | math"} and *still* require fewer parentheses than
using a minus sign!

```{.unwrap pipe="sh | math block"}
{
  var 'x'
  {
    num '2'
    {
      num '1' | neg
      cat inner.mml
    } | mult
    num '96'
  } | add
} | mult
```

See [the page on negatives](negatives.html) for more discussion about
`1`{.unwrap pipe="num | neg | math"}.

## Alignment and spacing ##

Adding bars doesn't add any horizontal space, which makes it easier to align
things. For example, here's a small times-table written using bar notation. The
contents of each cell is an independent MathML expression: no attempt has been
made to align them, yet they manage to look pretty clear.

<style type="text/css">
.times-table {
 border-collapse: collapse;
 margin: auto;
}

.times-table > * > tr > th:first-child {
 border-right:black solid 1px;
}

.times-table > thead > tr > th {
 border-bottom:black solid 1px;
}

.times-table td, .times-table th {
 padding: 0.2em 0.3em;
}
</style>

```{pipe="cat > table.html"}
<table class="times-table"><thead>
 <tr class="even">
  <th>`<mo>×</mo>`{.unwrap pipe="math"}</th>
  <th scope="col">`2`{.unwrap pipe="num | neg | math"}</th>
  <th>`1`{.unwrap pipe="num | neg | math"}</th>
  <th>`0`{.unwrap pipe="num | math"}</th>
  <th>`1`{.unwrap pipe="num | math"}</th>
  <th>`2`{.unwrap pipe="num | math"}</th>
 </tr>
</thead><tbody>
 <tr class="odd">
  <th scope="row">`2`{.unwrap pipe="num | neg | math"}</th>
  <td>`4`{.unwrap pipe="num | math"}</td>
  <td>`2`{.unwrap pipe="num | math"}</td>
  <td>`0`{.unwrap pipe="num | math"}</td>
  <td>`2`{.unwrap pipe="num | neg | math"}</td>
  <td>`4`{.unwrap pipe="num | neg | math"}</td>
 </tr>
 <tr class="even">
  <th>`1`{.unwrap pipe="num | neg | math"}</th>
  <td>`2`{.unwrap pipe="num | math"}</td>
  <td>`1`{.unwrap pipe="num | math"}</td>
  <td>`0`{.unwrap pipe="num | math"}</td>
  <td>`1`{.unwrap pipe="num | neg | math"}</td>
  <td>`2`{.unwrap pipe="num | neg | math"}</td>
 </tr>
 <tr class="odd">
  <th>`0`{.unwrap pipe="num | math"}</th>
  <td>`0`{.unwrap pipe="num | math"}</td>
  <td>`0`{.unwrap pipe="num | math"}</td>
  <td>`0`{.unwrap pipe="num | math"}</td>
  <td>`0`{.unwrap pipe="num | math"}</td>
  <td>`0`{.unwrap pipe="num | math"}</td>
 </tr>
 <tr class="even">
  <th>`1`{.unwrap pipe="num | math"}</th>
  <td>`2`{.unwrap pipe="num | neg | math"}</td>
  <td>`1`{.unwrap pipe="num | neg | math"}</td>
  <td>`0`{.unwrap pipe="num | math"}</td>
  <td>`1`{.unwrap pipe="num | math"}</td>
  <td>`2`{.unwrap pipe="num | math"}</td>
 </tr>
 <tr class="odd">
  <th>`2`{.unwrap pipe="num | math"}</th>
  <td>`4`{.unwrap pipe="num | neg | math"}</td>
  <td>`2`{.unwrap pipe="num | neg | math"}</td>
  <td>`0`{.unwrap pipe="num | math"}</td>
  <td>`2`{.unwrap pipe="num | math"}</td>
  <td>`4`{.unwrap pipe="num | math"}</td>
 </tr>
</tbody></table>
```

<figure>

```{.unwrap pipe="sh"}
< table.html pandoc -f markdown -t json | panpipe | panhandle
```

<figcaption>Multiplication table with overbars for negation</figcaption>
</figure>

Bars add a *little* vertical space to an expression, which causes a little
wiggling; but only due to the *thickness* of the line (and some separation). In
comparison, minus signs extend an expression horizontally by the *entire length*
of the line (plus some separation). Here's the same table using minus signs, and
the wiggling between cells is much more pronounced!

<figure>

```{.unwrap pipe="sh"}
< table.html sed -e 's/math/math minus/g' |
  pandoc -f markdown -t json | panpipe | panhandle
```

<figcaption>Multiplication table with minus signs for negation</figcaption>
</figure>

This seems like a minor quibble, but grids of numbers/expressions are used all
over the place: spreadsheets, long addition/multiplication, matrices, etc. When
things line-up, we can skim them quickly and spot discrepencies; when they
don't, we must expend mental effort to keep track of items individually.

### Negative digits ###

Thanks to the place-value system we use to write them, *numbers themselves* can
be thought of as a grid of digits. Negative numbers are thus made of
[negative digits](negative_digits.html), which can make arithmetic simpler and
more uniform. This is natural to express with overbar notation, but awkward and
ambiguous using minus signs.

## Known Conflicts ##

Bar notation is also used for things which have nothing to do with negatives,
which makes for unfortunate clashes. Here are some I'm aware of:

**Please suggest more clashes!**

### Repeating Decimals ###

```{pipe="cat > repeating_decimal_bar.mml"}
<mrow>
  <mn>1</mn>
  <mo>.</mo>
  <mover>
    <mn>23</mn>
    <mo>&#175;</mo>
  </mover>
</mrow>
```

```{pipe="cat > repeating_decimal_ellipsis.mml"}
<mrow>
  <mn>1.23232323</mn>
  <mo>…</mo>
</mrow>
```

Bars are sometimes used for repeating decimals, e.g.
`cat repeating_decimal_bar.mml`{.unwrap pipe="sh | math nosem"} to represent
`cat repeating_decimal_ellipsis.mml`{.unwrap pipe="sh | math nosem"}. Since
[this varies between
countries](https://en.wikipedia.org/wiki/Repeating_decimal#Notation), it can be
avoided in favour of a different notation.

### p-adic Numbers ###

p-adic numbers have the same conflict as repeating decimals, except going to the
left instead of right. If we pick one of the other common notations for
repeating decimals, we should also use it for p-adic numbers, for consistency.
