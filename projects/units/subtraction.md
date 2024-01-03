---
title: Problems with Subtraction
---

Subtraction is the operation of "taking away", and acts like the "opposite" of
addition. For example, we can use a subtraction to "undo" an addition:

$$(x + y) - y = x$$

This is a fine idea, but unfortunately subtraction is not as well-behaved as
addition, and I personally think it causes more problems than it solves.

## Subtraction is *Redundant* ##

We can define subtraction using addition and negation:

$$x - y \equiv x + \ngtv{y}$$

Hence situations involving negatives don't need subtraction: we can stick to
additions, and sprinkle in some negatives as needed. Furthermore, if a situation
*doesn't* allow negatives, then it doesn't allow subtraction either, since it
gives negative answers when the second value is larger than the first
(e.g. $5 - 7 = \ngtv{2}$).

So any time we might want subtraction, we could use negatives instead; and any
time we can't use negatives, we also can't use subtraction. Thus subtraction
isn't *needed*. We might find it *useful* . Unf subtraction is just an abbreviation

We can't swap around the values of a subtraction, since $x - y \neq y - x$. In
fact, we get the negative result: $x - y = \ngtv{y - x}$ (we say that
subtraction "anti-commutes").

Subtraction becomes ambiguous when there are more than two values. For example
$x - y - z$ could mean $(x - y) - z$ ("$x$ take away $y$, then take away $z$"),
*or* it could mean $x - (y - z)$ ("$y$ take away $z$, taken away from $x$).
These are different things, e.g. $(1 - 2) - 3 = \ngtv{1} - 3 = \ngtv{4}$,
whilst $1 - (2 - 3) = 1 - \ngtv{1} = 2$. In contrast can be
done We can switch the values Subtraction is not commutative

$$x - y \equiv x + \ngtv{y}$$
