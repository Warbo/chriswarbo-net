---
title: "Ivory: Adjoins And Quotients"
---

We've seen how [sums and products](sums_and_products.html) determine the
structure of Ivory expressions. Now we'll define the elements of these sums and
products.

## Adjoining ##

Each level of the Ivory tower includes the levels above. In algebraic terms,
each is an [extension](https://en.wikipedia.org/wiki/Field_extension) of the
level above, with extra values *adjoined*. The result of this "adjoining" is a
level containing:

 - All of the values of the level above.
 - The value we adjoined.
 - All sums and products of those values, AKA the *closure* of addition and
   multiplication.

The power of this approach comes from taking the closure. For example, we can't
turn `natural`{.scheme} into `integer`{.scheme} by appending each negative
number separately. Yet if we allow arbitrary products, we only need to append
`(- 1)`{.scheme} directly: every other negative `integer`{.scheme} will arise as
the product of `(- 1)`{.scheme} with an existing `natural`{.scheme}.

The following levels are extensions in this way:

 - `zero`{.scheme} adjoins the value `0`{.scheme} to `void`{.scheme}.
 - `natural`{.scheme} adjoins the value `1`{.scheme} to `zero`{.scheme}.
 - `integer`{.scheme} adjoins the value `(- 1)`{.scheme} to `natural`{.scheme}.
 - `dyadic`{.scheme} adjoins the value `(^ 2 (- 1))`{.scheme} to
   `integer`{.scheme}.
 - `sexagesimal` adjoins the values `(^ 3 (- 1))`{.scheme} and
   `(^ 5 (- 1))`{.scheme} to `dyadic`{.scheme}.
 - `geometric`{.scheme} adjoins values of the form `(i n)`{.scheme},
   `(h n)`{.scheme} and `(d n)`{.scheme} to `scalar`{.scheme}.
 - `



On its own, these

New element symbols are *adjoined* which introduces new symbols; and *quotients*.
