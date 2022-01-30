---
title: Negatives
parseArgs: []
renderArgs: [ "--mathjax" ]
extra_head:
  <script src="../../js/mathjax/tex-mml-chtml.js"></script>
  <script src="../../js/mathjax/polyfill.min.js"></script>
---

"Negatives" (like [negative
numbers](https://en.wikipedia.org/wiki/Negative_number)) are a great idea in
many situations, but they can be tricky to learn and are a common source of
mistakes. Here are some less-conventional ways to make them easier to work with,
and hopefully easier to understand.

Caveat: I'm talking about normal, everyday maths here; nothing exotic, like
finite fields or whatever!

<div style="display: none;">

$$
\def\ngtv#1{{\overline{#1}}}
$$

</div>

## Bar Notation ##

There are many ways we can write down a "negative". The most common way is using
a "minus sign", e.g. $-5$ for the negative of $5$, but I think [minus signs are
too cumbersome and confusing](minus.html). Instead, I prefer to draw a line
*over the top* (often called a "bar"), rather than to the left.

For example, the negative of $5$ can be written $\ngtv{5}$, the negative of a
variable $x$ can be written $\ngtv{x}$, the negative of a sum $p + 43$ can be
written $\ngtv{p + 43}$, and so on.

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

Bars are sometimes used for repeating decimals, e.g. $1.\ngtv{23}$ to represent
$1.23232323\ldots$. Since [this varies between countries
](https://en.wikipedia.org/wiki/Repeating_decimal#Notation), it can be avoided
in favour of a different notation.

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

$$x(2 + \ngtv{123.45 + \frac{42y^2}{7}} + 96)$$

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
Place-value numbers *themselves* are a grid of digits (which makes bar notation
so attractive when using [negative digits](negative_digits.html)).

Bars add a *little* vertical space to an expression, but only the *thickness* of
the bar (and some separation); in comparison, minus signs extend an expression
horizontally by their entire *length* (plus separation).

## Defining Negatives with Plus and Zero ##

We can define the "meaning" of negatives using the following property:

(@negative) $$x + \ngtv{x} \equiv 0$$

In other words, the negative of something is whatever we can *add* to that thing
to get a result of $0$.

Note that this definition *does not* involve subtraction, since [I consider
subtraction to be more trouble than it's worth](subtraction.html).

### The Negative of $0$ ###

We know that adding $0$ to anything will leave that thing unchanged ($0$ is
called the "identity" value for addition), i.e.

(@zero) $$x + 0 = x$$

We can replace $x$ with $0$, to get $0 + 0 = 0$, which matches our definition of
a negative above. Hence the negative of $0$ is itself $0$:

(@zero-bar) $$\ngtv{0} = 0$$

### "Double Negatives" (Negatives of Negatives) ###

If we take the negative of something that's already a negative, we get back the
original thing:

(@double-negative) $$\ngtv{\ngtv{x}} = x$$

<details class="odd">
 <summary>Proof</summary>

We can add $0$ to a "double negative" without changing it (as per equation
(@zero)):

$$\ngtv{\ngtv{x}} = \ngtv{\ngtv{x}} + 0$$

We can replace $0$ with the sum of a value and its negative (as per equation
(@negative)). Let's replace the $0$ with the sum of $x$ and its negative
$\ngtv{x}$:

$$\ngtv{\ngtv{x}} = \ngtv{\ngtv{x}} + x + \ngtv{x}$$

Let's rearrange this sum, and introduce parentheses to make the next step
clearer:

$$\ngtv{\ngtv{x}} = x + (\ngtv{x} + \ngtv{\ngtv{x}})$$

The expression in the parentheses is the sum of a value ($\ngtv{x}$) and its
negative ($\ngtv{\ngtv{x}}$), which is $0$ (according to equation (@negative)):

$$\ngtv{\ngtv{x}} = x + 0$$

Finally, we can get rid of the addition since it doesn't change the value (as
per equation (@zero)):

$$\ngtv{\ngtv{x}} = x$$

QED

</details>

Hence such "double negatives" will "cancel out".

### Expanding, Contracting and Shifting Negatives ###

When negatives are multiplied, we can "expand" the bar to cover the whole
multiplication:

(@expand) $$\ngtv{x}y = \ngtv{xy}$$

<details class="odd">
  <summary>Proof</summary>

Adding $0$ to our multiplication won't change its value (equation (@zero)):

$$\ngtv{x}y = \ngtv{x}y + 0$$

We can replace that $0$ with the sum of a value ($xy$) and its negative
($\ngtv{xy}$) as per equation (@negative):

$$\ngtv{x}y = \ngtv{x}y + xy + \ngtv{xy}$$

The first two terms have a common factor of $y$, which we can factor out:

$$\ngtv{x}y = (\ngtv{x} + x)y + \ngtv{xy}$$

Those parentheses contain the sum of a value ($x$) and its negation
($\ngtv{x}$), which is equal to $0$ (equation (@negative)):

$$\ngtv{x}y = 0y + \ngtv{xy}$$

Multiplying by $0$ always gives $0$:

$$\ngtv{x}y = 0 + \ngtv{xy}$$

Finally we can remove the addition (as per equation (@zero)):

$$\ngtv{x}y = \ngtv{xy}$$

QED

</details>

We can use the same approach for the second value (except we'll get a common
factor of $x$ in the proof), to show that:

(@expand-right) $$x\ngtv{y} = \ngtv{xy}$$

These equations also tell us we can "contract" a bar from an entire
multiplication down to just one of its factors.

In combination, these let us "shift" bars from one factor to another:

(@shift) $$\ngtv{x}y = x\ngtv{y}$$

### Multiplying Negatives ###

Consider the multiplication of *two* negatives: we can "expand" one of the bars
to cover the whole product (as per equation (@expand)):

$$\ngtv{x}\ngtv{y} = \ngtv{x\ngtv{y}}$$

From equation (@expand-right), we can replace $x\ngtv{y}$ with $\ngtv{xy}$:

$$\ngtv{x}\ngtv{y} = \ngtv{\ngtv{xy}}$$

This is a double negative, which cancels out (via equation (@double-negative)):

$$\ngtv{x}\ngtv{y} = xy$$

In this way, multiplications can always be simplified to contain either one
negative, or no negatives at all.

### The Role of $\ngtv{1}$ ###

We can always multiply values by $1$ without changing them, i.e. $x = 1x$ (we
say $1$ is the "identity" value for multiplication). This combines nicely with
the expansion, contraction and shifting of negatives:

$$\ngtv{x} = \ngtv{1x} = \ngtv{1}x$$

This lets us think about a value's "direction" ($1$ or $\ngtv{1}$) separately
to its "size"; for example, whether a number is pointing "up" or "down" the
number line.

### Normal (Canonical) Forms ###

Having multiple ways to write the same thing can be convenient during a
calculation, but we'd rather have a *single* way to write our "final result"; so
it can be easily compared to other results (by just comparing the symbols).

Such notation is called a "normal form" (or "canonical form"), and may be
familiar from dealing with fractions, where e.g. $\frac{27}{54}$ and
$\frac{3}{6}$ *appear* to be different numbers. We can write these in normal
form by removing common factors, to get $\frac{1}{2}$ and $\frac{1}{2}$; which
are obviously identical.

In the case of products, we can introduce and eliminate factors of $1$, and we
can expand, contract and shift around negatives between factors. Hence there are
many ways to write down the same value, and it would be nice to have a normal
form for any "final results". The most natural approach is the following:

 - Eliminate any factors of $1$. This makes the expression as small as possible.
 - Expand all negatives to cover the whole expression, since this avoids having
   to choose between factors arbitrarily.
 - Eliminate any "double negatives", to get only zero or one negative. This is
   easy once all the negatives have been expanded.

For example, the following expression:

$$3\ngtv{x}(\ngtv{y} + z) + \ngtv{x\ngtv{y}}$$

Would have the following normal form:

$$\ngtv{3x(\ngtv{y} + z)} + xy$$

Note that the $\ngtv{y} + z$ isn't itself a product, so its negative can't
expand or shift around. We could also "multiply out" and factorise, but that's
more about "sums and products" rather than negatives, so I'll leave it up to
taste, e.g.

$$x(4y + \ngtv{3z})$$
