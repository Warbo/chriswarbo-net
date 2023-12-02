---
title: "Ivory: A Mammoth Numerical Tower"
---

<figure>

```
             ┌─┐ ┌──┐ ┌─┐
             │ └─┘  └─┘ │
             │   zero   │
            ╭┴──────────┴╮
            │  boolean   │
           ╭┴────────────┴╮
           │   natural    │
          ╭┴──────────────┴╮
          │    integer     │
         ╭┴────────────────┴╮
         │     rational     │
        ╭┴──────────────────┴╮
        │     algebraic      │
        ├────────────────────┤
        |        real        |
       ╭┴────────────────────┴╮
       │      geometric       │
       ├──────────────────────┤
       │        number        │
      ┌┴┐ ┌─┐ ┌──┐  ┌──┐ ┌─┐ ┌┴┐
      │ └─┘ └─┘  └──┘  └─┘ └─┘ │
      │  univariate-monomial   │
     ╭┴────────────────────────┴╮
     │        polynomial        │
    ┌┴┐ ┌─┐ ┌─┐  ┌──┐  ┌─┐ ┌─┐ ┌┴┐
    │ └─┘ └─┘ └──┘  └──┘ └─┘ └─┘ │
    │    rational-expression     │
   ╭┴────────────────────────────┴╮
   │     algebraic-expression     │
   ├──────────────────────────────┤
   │          expression          │
───┴──────────────────────────────┴───
```

 <figcaption>

The Ivory Tower (work in progress), showing numerical types and their
containment relationships. Each level of the tower contains everything above
it; wider levels contain more values.

 </figcaption>
</figure>

Ivory is a "numerical tower": a structured approach to implementing numbers and
arithmetic on a computer, which makes them amenable to programming in a rather
simple way. Compared to other numerical towers, Ivory removes a few levels and
adds many more. Whilst designed for computer programmers, these explanations and
descriptions may also be of use to non-programmers (feel free to skip the
implementation details!).

## Goals ##

Ivory aims to be more than yet another library of standalone functions/classes.
It instead takes a holistic view about incorporating useful mathematics more
deeply into a language. Its design is inspired by the numerical system of
[Scheme](https://www.scheme.org), and uses the language extension mechanisms of
[Racket](https://racket-lang.org) to support a wider variety of notation.

Ivory's focus is on exactness and correctness, since it is hard to build
software on unstable foundations. This precludes support for approximate formats
like IEEE754 floats, unums, posits, etc. (we can still convert to such formats,
but they are not in the tower; just like, say, strings are not in the tower but
we can convert to them).

As a numerical tower, Ivory is focused on the representation of numbers, and
designing a *one-dimensional nesting* of those representations, as seen in the
figure above. The main design principle for choosing what to put in the tower is
the existence of a unique *normal form* for each number, such that two numbers
are only equal when their representations are identical. This rules out very
general representations, like [closed-form
expressions](https://en.wikipedia.org/wiki/Closed-form_expression), [power
series](https://en.wikipedia.org/wiki/Power_series) and [computable
numbers](https://en.wikipedia.org/wiki/Computable_number), since their equality
is undecidable. This requirement for exact, unique representations is why Ivory
does not currently include transcendental numbers like τ and 𝑒, or operations
like $sin$ and $log$.

The levels `real`, `number` and `expression` provide no extra values or
operations, and act merely as alternative names for the levels above them
(`algebraic`, `geometric` and `algebraic-expression`, respectively). They
mark important transitions in the tower, below which some important results can
no longer be assumed to hold. For example, values above the `real` level can be
totally ordered into a "number line", whilst such comparisons don't generally
hold for the levels below. Applications which need comparable numbers should
check if they're `real`, rather than `algebraic`; that way, any extra levels
inserted between these in future revisions will be automatically supported.

## Required Knowledge ##

These pages do not assume any knowledge of advanced mathematics, but do assume
*some* familiarity with high-school algebra, like how to multiply groups of
terms, e.g.

$$(2 + a)(5 + 3b) = 10 + 5a + 6b + 3ab$$

We will be writing a lot of [s-expressions](/blog/2017-08-29-s_expressions.html)
(AKA Lisp syntax or [prefix
notation](https://en.wikipedia.org/wiki/Polish_notation)) since that matches the
Scheme programming language used by Ivory's reference implementation. This style
may be unfamiliar, but is pretty simple and has the benefit of removing
[ambiguities like
precedence](https://en.wikipedia.org/wiki/Order_of_operations) (which tend to
plague those trying to learn mathematics!).

In an s-expression, operations are written `(in parentheses)`{.scheme}, with the
operation's name/symbol first and its inputs after. For example, the above
equation could be written as an s-expression like this (I've lined up the two
inputs of `=`{.scheme} for clarity, but the meaning doesn't depend on layout):

```scheme
(= (× (+ 2 a) (+ 5 (× 3 b)))
   (+ 10 (× 5 a) (× 6 b) (× 3 a b)))
```

At times we'll be using Scheme's `quote`{.scheme} and `quasiquote`{.scheme}
features, which may need a little explanation for those who aren't used to
Scheme programming. A "quotation" is prefixed by a single-quote/apostrophe
`'`{.scheme}, and is used to represent data without attempting to execute
it. For example, the value of `'(+ 2 5)`{.scheme} is a *list* containing three
elements (like `["+", 2, 5]`{.python} in other languages; and *unlike*
`(+ 2 5)`{.scheme}, whose value is the `number`{.scheme} `7`{.scheme}). A
"quasiquotation" is prefixed by a backtick `` ` ``{.scheme}, and is like a
quotation except that expressions prefixed with a comma `,`{.scheme} are
"unquoted"; for instance `` `(10 plus 20 is ,(+ 10 20)) ``{.scheme} gives the
list `(10 plus 20 is 30)`{.scheme} (quasiquoting is similar to "string splicing"
in other languages, like `"10 plus 20 is ${10 + 20}"`{.php}).

## Implementation ##

This project is split across several pages, which describe the tower's design,
the basic mathematics and applications of its components, as well as
implementing them in the Racket programming language. The most interesting parts
are shown in a literate programming style using
[active code](/projects/activecode). The full Racket code is always linked at
the bottom of each page; alongside a "view source" link to the page's Markdown.

The following pages give introductory information, without defining any code:

 - [Numerical Towers](numerical_towers.html) introduces the idea, its tradeoffs,
   and contrasts it with other approaches to implementing arithmetic.
 - [Numbers In Scheme And Racket](numbers_in_scheme.html) describes the
   numerical towers defined by the Scheme standards and the Racket language's
   standard library, which Ivory builds on.

These pages introduce some of the mathematical ideas that guide our design, and
implement useful definitions that make the subsequent sections easier:

 - [Negatives And Inverses](negatives_and_inverses.html) uses these concepts to
   implement subtraction and division in terms of addition and multiplication,
   simplifying how much arithmetic we need to consider.
 - [Sums And Products](sums_and_products.html) implements these operations
   symbolically, explores the resulting tree structures, and defines algorithms
   to normalise them. Many of our tower's levels are based on these trees.

The levels of the Ivory tower are explained and implemented in the following:

 - [Zero, One, Many](zero_one_many.html) describes the top levels of the tower.
 - [Radicals](radicals.html) implements the `algebraic` level, representing
   roots symbolically and taking these into account when normalising.
 - [Geometric Units](geometric_units.html) extends the usual number line with
   extra values which anti-commute, beginning our implementation of the
   `geometric` level. These are represented symbolically, and our normalisation
   procedure is extended sort products alphabetically.
 - [Complex And Hypercomplex Numbers](complex_and_hypercomplex_numbers.html)
   explores the mezzanine levels inside `geometric`, and their various uses.
 - [Geometric Algebra](geometric_algebra.html) completes the implementation of
   the `geometric` level, and explores applications to geometry and physics.
 - [Indeterminates](indeterminates.html) implements the `univariate-monomial`,
   level, and introduces the `monomial` and `homogeneous` mezzanines from
   `polynomial`.
 - [Polynomials](polynomials.html) finishes the implementation of the
   `polynomial` level and explores its `univariate` mezzanine.
 - [General Expressions](general_expressions.html) extends `polynomial` to form
   the `rational-expression` and `algebraic-expression` levels, which are
   the most general numerical values supported by Ivory.