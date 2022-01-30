---
title: Problems with Minus
---

It's common to write negatives using a "minus sign", e.g. the negative of $5$ as
$-5$ and the negative of $x$ as $-x$. Here are some reasons I prefer the "bar"
notation.

## Minus Signs are Ambiguous ##

The minus sign has *two* common usages:

 - A *single* value prefixed with a minus sign indicates a negative, e.g. $-5$
   means the same as $\ngtv{5}$. (This is sometimes called "unary minus",
   since it applies to a single value)
 - A *pair* of values separated by a minus sign indicates *subtraction*, e.g.
   $5-2$ means "five take away two".

This is a reasonable distinction *on its own*, but causes problems when
expressions involve multiple values and operations. Whilst these can be avoided
using various conventions and workarounds, they make it harder to learn, harder
to teach, harder to program into a computer (especially if we don't want extra
parentheses "just in case"!), etc.

For example:

 - It's common to write multiplication by putting values side-by-side, e.g. $2x$
   to mean "$2$ times $x$". Minus signs make this ambiguous, since $2-x$ could
   mean "two times negative $x$" *or* it could mean "two take away $x$"; these
   are two *very* different things! Convention is to always treat such minus
   signs as subtraction, and use parentheses if we want multiplication, e.g.
   $2(-x)$

## Problems with Unary Minus ##

The usual way to indicate negatives is with a "minus sign" $-$, but that
notation can be confusing, and misses somenegatives by writing a lbar

things (where "positive" just means normal/standard/usual/everyday/etc.).

 - If a situation is easily modelled using Nat then we should stick to that, and
not introduce negative numbers, fractions, complex numbers, etc. since they're
not needed.
    - Example: Alice has 2 apples, Bob gives Alice 3 apples, how many apples
does Alice have?
    - Alice can't have 'negative apples', and Bob can only increase their apple
count, so negatives aren't needed (and likewise for fractional apples,
imaginary apples, infinitesimal apples, transfinite cardinals of apples,
etc.)
    - Note that many programming languages get this wrong: e.g. having 'length'
return a Z, forcing callers to account for the negative case.
 - When a situation *does* make sense with negatives, we *should* use them.
    - For example: if Alice has 3 apples and gives 2 to Bob, how many apples
does Alice have left?

## Problems with Binary Minux ##
