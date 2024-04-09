---
title: "SK in egglog: part 4"
---

This post continues my explorations with egglog, using it to implement SK logic.
Here's the code so far, with some `ruleset` annotations sprinkled in to give us
more control over what to `run`:

```
; Applicative combinatory logic. We use (C "foo") for constants, which will show
; up in the e-graph and output of egglog.
(datatype Com
          (App Com Com)
          (C String))

; Base combinators S and K
(let S (C "S"))
(let K (C "K"))

; Example combinators
(let I     (C "I"    ))
(let TRUE  (C "TRUE" ))
(let FALSE (C "FALSE"))
(let IF    (C "IF"   ))
(union I (App (App S K) K))
(union TRUE  K)
(union FALSE (App S K))
(union IF    I)

; Rewrite rules for running SK expressions
(ruleset reduce)
(rewrite         (App (App K x) y) x                         :ruleset reduce)
(rewrite (App (App (App S x) y) z) (App (App x z) (App y z)) :ruleset reduce)
```

The last couple of posts explored the idea of extensionality, using Haskell to
uncover a problem I was running into in egglog. The cause turned out to be using
symbolic inputs to check if expressions agreed, when those expressions may have
already contained those symbols.

Now we're switching back to egglog, our first task is to identify whether or not
an expression contains uninterpreted symbols.

## Concreteness ##

We'll call expressions which contain *no* symbols "concrete" (we could also call
them "closed" or "variable-free", but remember that SK itself doesn't support
variables!). We'll model this in egglog using a "relation", a partial function
from its inputs (in this case a single `Com` value) to unit values:

```
(ruleset annotations)
(relation concrete (Com))
```

We'll define `concrete` in a bottom-up way: the basic combinators `S` and `K`
are `concrete`, which we can state as facts:

```
(concrete S)
(concrete K)
```

If two `Com` values `x` and `y` are `concrete`, then their combination
`(App x y)` is also concrete. It's tempting to do the obvious thing and make a
rule like this:

```
(rule ((concrete x)
       (concrete y))
      ((concrete (App x y)))
      :ruleset annotations)
```

However, this naïve definition introduces a *new* `Com` value `(App x y)` for
*every* existing pair of `Com` values, and this will keep going until our
database blows through our memory.

Instead of such unbounded growth, we want our egglog algorithms to (a) run on
"real" values, like our examples `I`, `TRUE`, `FALSE` and `IF` (rather than
self-generated inanities), and (b) terminate after a finite number of steps. To
achieve this we'll use a separate relation called `try`, which we'll only apply
to expressions that we actually want to check for extensional equality. Once an
expression is marked with `try`, it will propagate top-down to every component:

```
(relation try (Com))
(rule ((try (App x y)))
      ((try x)
       (try y))
      :ruleset annotations)
```

Now we can restrict our `rule` for `concrete`, so that it only propagates
upwards to combinations that are already marked as `try`:

```
(rule ((try (App x y))
       (concrete x)
       (concrete y))
      ((concrete (App x y)))
      :ruleset annotations)
```

To see how these work together, consider what will happen if we assert `try` for
some particular expression, like `(try I)`:

 - The initial database contains the facts `(concrete S)` and `(concrete K)`,
   with no more work left to do.
 - Once we state `(try I)`, we'll need to `run` the `annotations` `ruleset` for
   a few steps, to propagate this new information.
 - Since `I` is equal to `(App (App S K) K)`, asserting `(try I)` will cause the
   condition `(try (App x y))` to match, with `x` as `(App S K)` and `y` as `K`.
    - This causes `try` to propagate down, asserting the facts `(try (App S K))`
      and `(try K)`.
    - Nothing else becomes `concrete`, since that `rule` also requires
      `(concrete x)`, but we do not have `(concrete (App S K))`.
 - `(try K)` does not match the form `(try (App x y))`, so it doesn't lead to
   any further propagation.
 - `(try (App S K))` *does* have the required form, hence:
    - `try` propagates to its children, giving `(try S)` (we already had
      `(try K)`, so that's unchanged).
    - `concrete` can also propagate upwards, since `(concrete x)` and
      `(concrete y)` both hold, giving `(concrete (App S K))`.
 - The latter satisfies the `(concrete x)` condition that previously failed, so
   `concrete` propagates further to give `(concrete (App (App S K) K))`, i.e.
   `(concrete I)`.
 - At this point we're finished, since `try` has propagated down to all of the
   sub-expressions of `I`, and `concrete` cannot propagate upwards any further
   (until more `try` statements are asserted).

In fact, egglog can propagate these relations even further, since it's always
dealing with *equivalence classes* rather than specific values. In particular,
we may have defined `I` as `(App (App S K) K)`; but its equivalence class may
contain other expressions of the form `(App x y)`. Those will *also* be matched
against `(try (App x y))` and hence propagate `try` to *their* sub-expressions,
and anything those sub-expressions are equal to, and so on; and that, in turn,
allows `concrete` to bubble-up through all of those `try` `annotations`.
Although given the rules we've defined so far, the equivalence class for `I`
does not *yet* contain any other expressions of the form `(App x y)`!

The important point is that we can use the top-down `relation` `try` as an
annotation to control the bottom-up calculation of `concrete`, ensuring the
whole thing terminates after analysing the expressions we care about. We can
ensure this is working by performing a few checks; firstly that `concrete`
isn't propagating in the absence of any `try` statements:

```
(run annotations 10)
(check (concrete I))
```

Next we can assert `(try I)`, and ensure that `concrete` *does* propagate:

```
(try I)
(run annotations 10)
(check (concrete I))
```

Finally, we can check that `concrete` stopped propagating once it reached `I`:

```
(check (concrete (App K I)))
```

### Symbolicness ###

The opposite of `concrete` is `symbolic`:

```
(relation symbolic (Com))
```

Unlike Haskell, where we represented `symbolic` inputs with a `Stream`{.haskell}
of explicit `String`{.haskell} values; in egglog we can simply `declare` their
existence, without pinning down any precise value. We still want an unlimited
amount of distinct symbols, so we'll count them using a Grassman/Peano-encoding:

```
; Define symbolic inputs, whose precise values are not defined. Since we do not
; know the values of 'X', '(nextSymbol X)', '(nextSymbol (nextSymbol X))', etc.
; they must all be treated as separate entities.
(declare X Com)
(function nextSymbol (Com) Com)

(symbolic X)
```

We would like a `rule` that `(symbolic x)` implies `(symbolic (nextSymbol x))`,
but that would blow up the memory, similar to the problem we had for `concrete`.
Instead, we'll defer such assertions until we're performing an actual check for
extensional equality, which will limit the amount of `symbolic` values to
exactly as many as we actually need.

## Single-input extensionality ##

The simplest case of extensional equality is when two `concrete` expressions
give equal results for a single `symbolic` input:

```
(ruleset single-extensional)
(rule ((= (App f x) (App g x))
       (concrete f)
       (concrete g)
       (symbolic x))
      ((union f g))
      :ruleset single-extensional)
```

This `rule` is fine, but it will never get triggered when using our model to
analyse real, `concrete` expressions: it requires some `symbolic` values, but we
haven't defined any way to introduce those into our database. The following
`rule` does just that:

```
(rule ((= (App f x) (App g x)))
      ((try f)
       (try g))
      :ruleset single-extensional)
```

This `rule` gets triggered whenever two expressions happen to agree on a
particular input value. It does two important things:

 - It applies both of those expressions to the `symbolic` input `X`, introducing
   `symbolic` expressions into the database which can be picked up by the
   previous `rule`.
 - It also asserts `try` on both of those expressions, allowing `annotations` to
   propagate and satisfy those `concrete` pre-conditions.

The problem with such a simplistic approach is that it will only work for
expressions which agree on *one* input (the explicit symbol `X`). If `foo` and
`bar` are extensionally equal on, say, three inputs, then this analysis will
spot that `(App (App foo S) K)` is extensionally equal to `(App (App bar S) K)`,
that `(App (App foo K) (App S S))` equals `(App (App bar K) (App S S))`, and so
on; but it will fail to spot the general pattern that `(App (App foo X) Y)`
equals `(App (App bar X) Y)`, and hence that `foo` equals `bar`.

## Multiple-input extensionality ###

To generalise our extensionality check, we need to generalise the pre-condition
from `(concrete f)` and `(concrete g)`, to handle cases where `f` and `g` are
themselves applications involving `symbolic` inputs. We need to be careful here,
to avoid choosing a `symbolic` input which already appears inside one of these
expressions (since that broke my first attempt at implementing extensionality,
described in the previous post)!

We can do this by transforming all of the `symbolic` values in an expression,
wrapping each in `nextSymbol`: so `X` becomes `(nextSymbol X)`,
`(nextSymbol X)` becomes `(nextSymbol (nextSymbol X))`, etc. This
[preserves the meaning](https://ncatlab.org/nlab/show/alpha-equivalence) of the
expression, whilst guaranteeing that the symbol `X` does not to appear on its
own (without a `nextSymbol` wrapper), and is hence available to use as our next
`symbolic` input without conflict:

```
(ruleset extensional)
(function bumpSymbols (Com) Com)

; 'App' isn't 'symbolic', so recurse into its children. We require '(try c)' to
; avoid immediately matching our output, and hence iterating forever.
(rule ((= c (App f x))
       (try c))
      ((union (bumpSymbols c)
              (App (bumpSymbols f) (bumpSymbols x))))
      :ruleset extensional)

; A 'concrete' value contains no 'symbolic' values, so is unaffected.
(rule ((= c (bumpSymbols x))
       (concrete x))
      ((union  c x))
      :ruleset extensional)

; Wrap a 'symbolic' value in 'nextSymbol', and make sure to assert that is also
; 'symbolic'.
(rule ((= c (bumpSymbols x))
       (symbolic x))
      ((union c (nextSymbol x))
       (symbolic (nextSymbol x)))
      :ruleset extensional)
```

We can use `bumpSymbols` to implement the following `(sameInputs x y)` relation,
which is more general than separate `(concrete x)`/`(concrete y)` predicates:

```
; Whether two values involve the same 'symbolic' inputs (and no other symbols)
(relation sameInputs (Com Com))
(rule ((sameInputs x y)) ((sameInputs y x)) :ruleset extensional) ; Commutative

; Base case: 'concrete' values contain the same 'symbolic' inputs (namely: none)
(rule ((concrete x) (concrete y))
      ((sameInputs x y))
      :ruleset extensional)

; 'bumpSymbols' preserves the 'sameInputs' relation, whilst also making 'X'
; available as a fresh, unused 'symbolic' input we can add to both expressions.
; We use 'try' to cut-off recursion, as before.
(rule ((sameInputs f g)
       (try f) (try g))
      ((sameInputs (App (bumpSymbols f) X)
                   (App (bumpSymbols g) X)))
      :ruleset extensional)
```

## Unused arguments ##

## Cleaning up ##

## Church booleans ##
