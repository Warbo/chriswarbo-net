---
title: "SK logic in egglog: part 2, extensionality"
packages: [ 'egglog', 'graphviz' ]
---

```{pipe="cat > show && chmod +x show"}
#!/bin/sh
tee -a "$1"
echo >> "$1"
```

```{pipe="cat > hide && chmod +x hide"}
#!/bin/sh
./show "$@" > /dev/null
```

This follows on from [part 1](/blog/2024-02-25-sk_logic_in_egglog_1.html) where
we used [egglog](https://github.com/egraphs-good/egglog) (a hybrid of datalog
and equality-saturation) to implement
[combinatory logic](https://esolangs.org/wiki/Combinatory_logic). In that post
I showed how to encode combinators as a `datatype`, in a way that gives readable
outputs; how to specify simple equations, using `union` and `rewrite`; how to
query, `check` and display the database; and we used these to implement the
reduction rules (analogous to β-reduction) for the universal combinators `S` and
`K`.

Those reduction rules essentially tell us how to "run" an SK expression. This
time I want to go further, using the capabilities of egglog to discover
*indirect* equivalences between expressions. In particular, I want to implement
[(functional) extensionality]().

## Extensionality ##

Two expressions are said to be extensionally equivalent/equal when they produce
the same results as each other regardless of what input they're applied to. For
example, the expressions `(App (App S K) K)`{.scheme} and
`(App (App S K) S)`{.scheme} are extensionally equivalent, since they both act
like the identity function (returning whatever they're applied to as-is). Here's
how they reduce, for any arbitrary input represented as `a`:

<figure>

```scheme
(App (App (App S K) K) a)
(App (App K a) (App (K a)))  ; Reduced using S rule
a  ; Reduced using K rule (applied to the first K)
```

<figcaption>
Reduction of `(App (App S K) K)`{.scheme} on some arbitrary input `a`.
</figcaption>
</figure>

<figure>

```
(App (App (App S K) S) a)
(App (App K a) (App S a))  ; Reduced using S rule (applied to the first S)
a  ; Reduced using K rule
```

<figcaption>
Reduction of `(App (App S K) S)`{.scheme} on some arbitrary input `a`.
</figcaption>
</figure>

The reason these act the same is that by the second line, their differences only
appear in the second argument of `K`; and the reduction rule for `K` discards
that second argument. In fact, this makes *all* combinators of the form
`(App (App S K) foo)`{.scheme} extensionally equivalent to each other!

Extensional equivalence is trickier than β-equivalence (which is essentially
mechanical), since we need to demonstrate that both forms are equivalent
*regardless* of the choice of argument (i.e. *for all* arguments; known as a
[universal quantification]()).

Extensional equivalence is a key concept in software optimisation, since an
optimised program is *different* to the original, yet it must give the same
result as the original for all inputs. In other words, optimised programs must
be *extensionally equivalent* to their originals, whilst (hopefully!) using
fewer resources. A classic example is the set of programs "multiply input by
two", "add input to itself" and "left-shift input by one":

 - These are objectively *different*, since they each invoke a different
   operation (multiplication, addition, bitshift)
 - These will all produce the same result when applied to any (natural number)
   argument, making them *extensionally equivalent*
 - Those operations can give different "non-functional" behaviour, like resource
   usage, e.g. their decreasing circuit complexities

## Quick set up ##

We'll be continuing on from the previous example, but I'm going to make a few
tweaks. In particular, we'll put the `S` and `K` reduction rules into an egglog
"`ruleset`" called `reduce`: this lets us `run` these rules separately to others
later on. The changes look like this:

```scheme
; Applicative combinatory logic. We use (C "foo") for constants, which will show
; up in the e-graph and output of egglog.
(datatype Com
          (App Com Com)
          (C String))

; S and K combinators, and their rewrite rules
(let S (C "S"))
(let K (C "K"))

(ruleset reduce)
(rewrite (App (App K x) y) x :ruleset reduce)
(birewrite (App (App (App S x) y) z) (App (App x z) (App y z)) :ruleset reduce)
```

Notice that I've also changed the `S` rule to use `birewrite`: using `rewrite`
makes egglog generate a term like `(App (App x z) (App y z))`{.scheme} when an
existing term matches the form `(App (App (App S x) y) z)`{.scheme}; using
`birewrite` will generate terms *both ways*, introducing `S` combinators when
appropriate.

We can't use `birewrite` for the `K` rule since the second argument doesn't
appear on the right-hand-side, so egglog complains it's unbound. We can instead
use a collection of rules to handle each possible combination. I've put these in
a separate `ruleset` called `k-expand`, since they tend to blow up the database:

```scheme
; For any expressions x and y, expand x into (App (App K x) y). This first
; rule handles base cases:
(ruleset k-expand)
(rule ((= x (C a)) (= y (C b)))
      ((union x (App (App K x) y)))
      :ruleset k-expand)

; This rule handles any interactions between an App and a base case:
(rule ((= x (C a)) (= y (App b c)))
      ((union x (App (App K x) y))
       (union y (App (App K y) x)))
      :ruleset k-expand)

; This rule handles combinations of App
(rule ((= x (App a b)) (= y (App c d)))
      ((union x (App (App K x) y)))
      :ruleset k-expand)
```

## Universal quantification via symbolic computation ##

The hardest part of implementing extensional equivalence is proving universal
quantification, i.e. going from "equivalent for these inputs" to "equivalent for
*all* inputs". The approach we're going to take is [symbolic computation]():
we'll apply expressions to an "uninterpreted symbol" `V`, which we *declare* but
leave completely undefined. If `(App f X)`{.scheme} is equivalent to
`(App g X)`{.scheme} then `f` and `g` are extensionally equivalent.

### Checking our assumption using falsify ###


```
(declare V Com)
```

The existing rules for `S` and `K` will reduce expressions *involving* `X`; but
since egglog doesn't know what `X` is, it cannot reduce `X` itself or
applications of `X`. For example:

```
(run 10)
(check (= (App (App K X) K) X))
(check (= (App (App K S) X) S))
(query-extract
```

We
