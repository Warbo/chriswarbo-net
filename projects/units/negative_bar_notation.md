---
title: Bar Notation for Negation
---

There are many ways we can write down "negatives", or more general "negations"
(whose result may actually be positive!). The most common way is using a "minus
sign", e.g. `5`{.unwrap pipe="num | neg | math minus"} for the negative of
`5`{.unwrap pipe="num | math"} but I think [minus signs are too cumbersome and
confusing](minus.html). Instead, I prefer to draw a line *over the top* (often
called a "bar"), rather than to the left.

```{pipe="add > sum"}
<ci>p</ci><cn>43</cn>
```

For example, the negative of `5`{.unwrap pipe="num | math"} can be written
`5`{.unwrap pipe="num | neg | math"}, the negative of a variable
`x`{.unwrap pipe="var | math"} can be written
`x`{.unwrap pipe="var | neg | math"}, the negative of a sum
`cat sum`{.unwrap pipe="sh | math"} can be written
`cat sum`{.unwrap pipe="sh | neg | math"}, and so on.

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

### Known Conflicts ###

Bar notation is also used for things which have nothing to do with negatives,
which makes for unfortunate clashes. Here are some I'm aware of:

**Please suggest more clashes!**

#### Repeating Decimals ####

```{pipe="cat > repeating_decimal_bar"}
<mrow>
  <mn>1</mn>
  <mo>.</mo>
  <mover>
    <mn>23</mn>
    <mo>&#175;</mo>
  </mover>
</mrow>
```

```{pipe="cat > repeating_decimal_ellipsis"}
<mrow>
  <mn>1.23232323</mn>
  <mo>…</mo>
</mrow>
```

Bars are sometimes used for repeating decimals, e.g. `cat
repeating_decimal_bar`{.unwrap pipe="sh | math"} to represent `cat
repeating_decimal_ellipsis`{.unwrap pipe="sh | math"}. Since [this varies
between countries ](https://en.wikipedia.org/wiki/Repeating_decimal#Notation),
it can be avoided in favour of a different notation.

#### p-adic Numbers ####

p-adic numbers have the same conflict as repeating decimals, except going to the
left instead of right. If we pick one of the other common notations for
repeating decimals, we should also use it for p-adic numbers, for consistency.

### Pronunciation ###

I propose sticking to the word "negative" when saying these quantities or
expressions out loud, rather than pronouncing the symbols (like "minus five" or
"five bar").

### Long and Short Bars ###

Consider a long expression like this:

```{pipe="cat > frac.mml"}
<apply>
  <divide/>
  <apply>
    <power/>
    <ci>y</ci>
    <cn>3</cn>
  </apply>
  <cn>7</cn>
</apply>
```

```{.unwrap pipe="sh | math block"}
{
  echo 'x' | var
  {
    echo '2' | num
    {
      echo '123.45' | num
      cat frac.mml
    } | add | neg
    echo '96' | num
  } | add
} | mult | tee >(cat 1>&2)
```

The bar clearly shows which parts are negative, without needing more parentheses
(although we can add them if desired). For comparison, using minus signs would
give this:

$$x(2 + (-(123.45 + \frac{42y^2}{7})) + 96)$$

If we don't like such long bars, we can instead multiply by $\ngtv{1}$ (note
that this *still* require fewer parentheses than a minus sign):

$$x(2 + \ngtv{1}(123.45 + \frac{42y^2}{7}) + 96)$$

See below for more discussion of $\ngtv{1}$.

### Horizontal Alignment ###

Adding bars doesn't add any horizontal space, which makes it easier to align
things. For example, here's a small times-table:

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
<table class="times-table"><thead>
 <tr class="even">
  <th>×</th>
  <th scope="col">2&#x305;</th>
  <th>1&#x305;</th>
  <th>0</th>
  <th>1</th>
  <th>2</th>
 </tr>
</thead><tbody>
 <tr class="odd">
  <th scope="row">2&#x305;</th>
  <td>4</td>
  <td>2</td>
  <td>0</td>
  <td>2&#x305;</td>
  <td>4&#x305;</td>
 </tr>
 <tr class="even">
  <th>1&#x305;</th>
  <td>2</td>
  <td>1</td>
  <td>0</td>
  <td>1&#x305;</td>
  <td>2&#x305;</td>
 </tr>
 <tr class="odd">
  <th>0</th>
  <td>0</td>
  <td>0</td>
  <td>0</td>
  <td>0</td>
  <td>0</td>
 </tr>
 <tr class="even">
  <th>1</th>
  <td>2&#x305;</td>
  <td>1&#x305;</td>
  <td>0</td>
  <td>1</td>
  <td>2</td>
 </tr>
 <tr class="odd">
  <th>2</th>
  <td>4&#x305;</td>
  <td>2&#x305;</td>
  <td>0</td>
  <td>1</td>
  <td>2</td>
 </tr>
</tbody></table>

This seems like a minor quibble, but grids of numbers/expressions are used all
over the place: spreadsheets, long addition/multiplication, matrices, etc.
Place-value numbers *themselves* are a grid of digits, which makes bar notation
so attractive when using [negative digits](negative_digits.html).

Bars add a *little* vertical space to an expression, but only the *thickness* of
the bar (and some separation); in comparison, minus signs extend an expression
horizontally by their entire *length* (plus separation).
