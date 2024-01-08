---
title: Bar notation for negation
---

There are many ways we can write down "negatives", or more general "negations"
(whose result may actually be positive!). The most common way is using a "minus
sign", e.g. `5`{.unwrap pipe="num | neg | math minus"} for the negative of
`5`{.unwrap pipe="num | math"} but I think [minus signs are too cumbersome and
confusing](minus.html). Instead, I prefer to draw a line *over the top* (often
called a "bar"), rather than to the left.

```{pipe="add > sum.mml"}
<ci>p</ci><cn>43</cn>
```

For example, the negative of `5`{.unwrap pipe="num | math"} can be written
`5`{.unwrap pipe="num | neg | math"}, the negative of a variable
`x`{.unwrap pipe="var | math"} can be written
`x`{.unwrap pipe="var | neg | math"}, the negative of a sum
`cat sum.mml`{.unwrap pipe="sh | math"} can be written
`cat sum.mml`{.unwrap pipe="sh | neg | math"}, and so on.

This [bar notation](https://en.wikipedia.org/wiki/Overline#Math_and_science)
is not original! Prior/similar uses include:

 - [Signed digits](https://en.wikipedia.org/wiki/Signed-digit_representation)
   often use bars to indicate negatives.
 - Bars used for (logical) negation, (complex) conjugation, (set) complement,
   and other sorts of "inverses" (these are related, since negatives are
   [additive inverses](https://en.wikipedia.org/wiki/Additive_inverse))
 - [Wildberger](https://www.wildegg.com) advocates bar notation for negatives
   (e.g. in their [YouTube videos](https://www.youtube.com/c/njwildberger))

**Please suggest more links!**

## Pronunciation ##

I propose sticking to the word "negative" when saying these quantities or
expressions out loud, rather than pronouncing the symbols (like "minus five" or
"five bar"). This is more explicit and understandable, regardless of notation.

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

The bar clearly shows which parts are negative, without needing more parentheses
(although we can add them if desired). For comparison, using minus signs would
give this:

```{.unwrap pipe="sh | math block minus"}
cat long.mml
```

If we don't like such long bars, we can instead multiply by
`1`{.unwrap pipe="num | neg | math"} (note that this *still* requires fewer
parentheses than a minus sign):

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

See below for more discussion of `1`{.unwrap pipe="num | neg | math"}.

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
