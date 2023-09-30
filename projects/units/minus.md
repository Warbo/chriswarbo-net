---
title: Minus Signs Are Ambiguous
---

The minus sign has *two* common usages:

 - A *single* value prefixed with a minus sign indicates a negative, e.g. $-5$
   means the same as $\ngtv{5}$. (This is sometimes called "unary minus",
   since it applies to a single value)
 - A *pair* of values separated by a minus sign indicates *subtraction*, e.g.
   $5-2$ means "five take away two". (This is sometimes called "binary minus")

This is a reasonable distinction *on its own*, but causes problems when
expressions involve multiple values and operations. This makes things harder to
learn, harder to teach, harder to program into a computer, etc.

For example, it's common to write multiplication by putting values side-by-side,
e.g. $2x$ to mean "$2$ times $x$". Using the minus sign for two different
operations makes this ambiguous, since $2-x$ could mean "two times negative $x$"
*or* it could mean "two take away $x$"; these are two *very* different things!
Convention is to always treat such minus signs as subtraction, and use
parentheses if we want multiplication, e.g.  $2(-x)$

## Avoid Unary Minus: Prefer Over-bars For Negatives ##

Compared to minus signs, ["over-bar" notation](negative_bar_notation.html) seems
to be more elegant for indicating negatives/opposites/inverses.

## Avoid Binary Minus: Add Negatives Instead Of Subtracting ##

Addition is more "well behaved" than subtraction (it is commutative,
associative, etc.), so it's generally better to
[add negatives instead of subtracting](subtraction.html).
