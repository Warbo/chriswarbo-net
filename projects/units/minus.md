---
title: Minus signs are ambiguous
---

The minus sign has *two* common usages:

 - A *single* value prefixed with a minus sign indicates a negative, e.g.
   `5`{.unwrap pipe="num | neg | math minus"}
   means the same as `5`{.unwrap pipe="num | neg | math"}. (This is sometimes
   called "unary minus", since it applies to a single value)
 - A *pair* of values separated by a minus sign indicates *subtraction*, e.g.
   `num '5'; num '2';`{.unwrap pipe="sh | mapply minus | math"} means "five take
   away two". (This is sometimes called "binary minus")

This is a reasonable distinction *on its own*, but causes problems when
expressions involve multiple values and operations. This makes things harder to
learn, harder to teach, harder to program into a computer, etc.

```{pipe="cat > explicit.mml"}
<mrow>
  <mn>2</mn>
  <mo>⁢</mo>
  <mrow>
    <mo>(</mo>
    <mrow><mo>-</mo><mi>x</mi></mrow>
    <mo>)</mo>
  </mrow>
</mrow>
```

For example, it's common to write multiplication by putting values side-by-side,
e.g. `num '2'; var 'x';`{.unwrap pipe="sh | mult | math"} to mean
"`2`{.unwrap pipe="num | math"} times `x`{.unwrap pipe="var | math"}". Using the
minus sign for two different operations makes this ambiguous, since
`num '2'; var 'x';`{.unwrap pipe="sh | mapply minus | math"} could mean "two
times negative `x`{.unapply pipe="var | math"}" *or* it could mean "two take
away `x`{.unwrap pipe="var | math"}"; these are two *very* different things!
Convention is to always treat such minus signs as subtraction, and use
parentheses if we want multiplication, e.g.
`cat explicit.mml`{.unwrap pipe="math nosem"}

## Avoid unary minus: prefer over-bars for negatives ##

Compared to minus signs, ["over-bar" notation](negative_bar_notation.html) seems
to be more elegant for indicating negatives/opposites/inverses. It also extends
naturally to the idea of [negative digits](negative_digits.html).

## Avoid binary minus: add negatives instead of subtracting ##

Addition is more "well behaved" than subtraction (it is commutative,
associative, etc.), so it's generally better to
[add negatives instead of subtracting](subtraction.html).
