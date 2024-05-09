---
title: "SK in egglog: part 4, extensional equality"
packages: [ 'egglog', 'graphviz', 'timeout' ]
---

```{pipe="cat > show && chmod +x show"}
#!/bin/sh
set -eu
F=${1:-sk.egg}
echo >> "$F"
tee -a "$F"
echo >> "$F"
```

```{pipe="cat > hide && chmod +x hide"}
#!/bin/sh
NAME=hide ./show "$@" > /dev/null
```

```{pipe="cat > run && chmod +x run"}
#!/bin/sh
set -e
{
  cat sk.egg
  echo
  cat "$1"
} > "run-$1"
MAX_SECS=120 withTimeout egglog "run-$1" 2> >(tee >(cat 1>&2)) || {
  CODE="$?"
  echo "Error running egglog run-$1, contents below"
  cat "run-$1"
  echo "End contents of run-$1"
  exit "$CODE"
} 1>&2
```

This post continues my explorations with egglog, using it to implement SK logic.
Here's the code so far, with some `ruleset` annotations sprinkled in to give us
more control over what to `run`:

```{.scheme pipe="./show"}
;; Applicative combinatory logic. We use (C "foo") for constants, which will
;; show up in the e-graph and output of egglog.
(datatype Com
          (App Com Com)
          (C String))

;; Base combinators S and K
(let S (C "S"))
(let K (C "K"))

;; Example combinators
(let I     (C "I"    ))
(let TRUE  (C "TRUE" ))
(let FALSE (C "FALSE"))
(let IF    (C "IF"   ))
(union I (App (App S K) K))
(union TRUE  K)
(union FALSE (App S K))
(union IF    I)

;; Rewrite rules for running SK expressions
(ruleset reduce)
(rewrite      (App (App K x) y)    x                         :ruleset reduce)
(rewrite (App (App (App S x) y) z) (App (App x z) (App y z)) :ruleset reduce)
```

The last couple of posts explored the idea of extensionality, using Haskell to
uncover a problem I was running into in egglog. The cause turned out to be using
symbolic inputs to check if expressions agreed, when those expressions may have
already contained those symbols.

Now we're switching back to egglog, our first task is to distinguish expressions
that contain such uninterpreted symbols.

## Representing symbols ##

We'll represent uninterpreted symbols numerically. One way to do this is using a
unary/Peano-style encoding, with a "zeroth symbol" and a function for the
"successor symbol". However, egglog has numbers built-in via its `i64`{.scheme}
`sort`{.scheme}, so we might as well use those (they're also backed by efficient
Rust code)! We'll turn these `i64`{.scheme} values into `Com`{.scheme} values
using an egglog `function`{.scheme} called `V`{.scheme} (for Variable; since
we're already using `S`{.scheme} for something else):

```{.scheme pipe="./show"}
(function V (i64) Com)
```

We'll avoid defining any outputs for `V`{.scheme}, so it remains uninterpreted:
that way egglog can treat `(V 0)`{.scheme}, `(V 1)`{.scheme}, etc. as
`Com`{.scheme} values, but it cannot make any further assumptions about their
structure or relationships.

We'll still use the idea of taking the "successor symbol", but that can now be
an ordinary function that increments our `i64`{.scheme} value:

```{.scheme pipe="./show"}
(ruleset symbols)
(function bumpSymbols (Com) Com)
```

```{.scheme pipe="./show bump-quiet.egg"}
(rewrite (bumpSymbols (V n)) (V (+ n 1))
         :ruleset symbols)
```

We'll also generalise this to work on *any* `Com`{.scheme} value. As the name
suggests, it will propagate recursively through the tree structure of a
`Com`{.scheme} expression, using the above `rewrite`{.scheme} to increment all
of the symbols it contains:

```{.scheme pipe="./show"}
(birewrite (bumpSymbols (App x y))
           (App (bumpSymbols x) (bumpSymbols y))
           :when ((bumpSymbols x) (bumpSymbols y))
           :ruleset symbols)
```

The condition `:when ((bumpSymbols x) (bumpSymbols y))`{.scheme} ensures
termination, by only acting on expressions that already appear in the
database. It's important that this uses `birewrite`{.scheme}, since we want to
ensure *uses* of `bumpSymbols`{.scheme} get propagated down through
sub-expressions; but we also need *known* expressions to coalesce their
`bumpSymbols`{.scheme} wrappers upwards, for our rules to match on.

Finally, `bumpSymbols`{.scheme} does nothing to leaves, since they contain no
symbols:

```{.scheme pipe="./show"}
(birewrite (bumpSymbols (C x)) (C x)
           :ruleset symbols)
```

<details class="odd">
<summary>"Printf debugging" in egglog…</summary>

When we execute an egglog script, it can be difficult to know what it's doing:
e.g. if some incorrect fact appears, we want some trace of the rules which lead
to it. Support for "proofs" may help, and this seems to be on egglog's wishlist,
but it's not yet available (as of 2024-04-24).

For very simple rules, we can sometimes reformulate them so each rule puts a
marker in its output. However, this can get pretty complicated and tedious for
larger rulesets; and requires effectively re-implementing many parts of egglog
inside itself, which wastes a lot of time.

If we just want to see that some `rule` has fired, we can use an `extract`
action to emit a message. The argument to `extract` can be any expression: the
most useful are plain strings (to indicate what's happened) and variables (to
see what values are being processed). For example, if we want to see how many
symbolic values we're creating, the following variant of the above `rewrite`
will show us:

```{.scheme pipe="./show bump-loud.egg"}
(rule ((bumpSymbols (V n)))
      ((let bumped (V (+ n 1)))
       (extract "Bumped to:")
       (extract bumped)
       (union (bumpSymbols (V n)) bumped))
      :ruleset symbols)
```

For example, analysing the current database contents until saturation:

```{.scheme pipe="./show bump-loud.egg"}
(run-schedule (saturate reduce symbols))
```

```{pipe="sh"}
set -e
./run bump-loud.egg
```

</details>

```{pipe="sh"}
./show < bump-quiet.egg > /dev/null
```

## Counting symbolic arguments ##

Extensional equality tells us that when two expressions like
`(App (App foo (V a)) (V b))`{.scheme} and
`(App (App bar (V a)) (V b))`{.scheme} are equal, then `foo`{.scheme} and
`bar`{.scheme} are equal; as long as neither contains `(V a)`{.scheme} or
`(V b)`{.scheme}. This is a bit trickier to represent in egglog, since we're
dealing with e-graphs rather than specific terms; yet there's a remarkably
simple way to capture the *essence* of this situation, by counting symbolic
arguments. Here's the `function`{.scheme} we'll use:

```{.scheme pipe="./show"}
(function symbolicArgCount (Com) i64 :merge (min old new))
```

We'll explain the `:merge:`{.scheme} clause in a moment (it's required to avoid
ambiguity). First we'll define the base case, that expressions involving *no*
symbols will have a `symbolicArgCount`{.scheme} of zero:

```{.scheme pipe="./show"}
(rule ((= (bumpSymbols x) x))
      ((set (symbolicArgCount x) 0))
      :ruleset symbols)
```

The pre-condition here requires that `bumpSymbols`{.scheme} leaves `x`{.scheme}
unchanged: we'll say that such values are "concrete". We know any `Com`{.scheme}
of the form `(C x)`{.scheme} is concrete, since that's stated by one of the
rules above; and from this we know `S`{.scheme} and `K`{.scheme} are
concrete. The rules for `bumpSymbols`{.scheme} also make `(App x y)`{.scheme}
concrete when both `x`{.scheme} and `y`{.scheme} are concrete. To see this,
consider what happens to `(bumpSymbols (App x y))`{.scheme}: the rules for
`bumpSymbols`{.scheme} say that is equal to
`(App (bumpSymbols x) (bumpSymbols y))`{.scheme}; since we're assuming
`x`{.scheme} and `y`{.scheme} are concrete (i.e. unaffected by
`bumpSymbols`{.scheme}) this equals `(App x y)`{.scheme}, which is the argument
we originally gave to `bumpSymbols`{.scheme}; hence `(App x y)`{.scheme} is
unaffected by `bumpSymbols`{.scheme}, and therefore is concrete. ∎

There's an interesting subtlety here, since it's really *equivalence classes*
that are concrete, and those *may* contain symbolic values! For example, it
seems obvious that `S`{.scheme} is concrete, yet it's equivalence class also
contains expressions like `(App (App K S) (V 0))`{.scheme}, since that reduces
to `S`{.scheme} and we've implemented our reduction rules using egglog's
equality. In such cases, the symbolic values are never *necessary*, since there
are equivalent expressions which don't contain them. This is why our
`:merge`{.scheme} clause resolves ambiguity by taking the *minimum* count, since
that's the more necessary/fundamental value.

Larger counts occur when we apply an expression to a symbol which doesn't occur
in that expression (or at least, isn't *necessary* for that expression). We can
ensure this using `bumpSymbols`{.scheme}: since its result has all its symbols
incremented, we *know* that it cannot contain `(V 0)`{.scheme} (at least, in a
way that's "necessary", as above).

```{.scheme pipe="./show"}
(rule ((= n (symbolicArgCount x))
       (App x y))
      ((set (symbolicArgCount (App (bumpSymbols x) (V 0)))
            (+ 1 n)))
      :ruleset symbols)
```

In this case we have two pre-conditions: `(App x y)`{.scheme} requires that our
database already contains `x`{.scheme} applied to *something*, which ensures the
recursive pattern `(App (bumpSymbols x) (V 0))`{.scheme},
`(App (App (bumpSymbols (bumpSymbols x)) (V 1)) (V 0))`{.scheme}, etc. will
eventually terminate. Secondly, we avoid using
`(+ 1 (symbolicArgCount x))`{.scheme}, since that can again lead to infinite
recursion; instead we use `(= n (symbolicArgCount x))`{.scheme} to restrict our
matches to values whose `symbolicArgCount`{.scheme} has already been calculated.

This definition of `symbolicArgCount`{.scheme} does two things for us: firstly,
it is only defined for expressions which apply a concrete expression to some
number of symbols with decrementing indices, e.g.
`(App (App (App (App S K) (V 2)) (V 1)) (V 0))`{.scheme}; secondly, in the cases
where it's defined, it tells us how many symbols there are.

This is enough for us to implement extensional equality!

## Extensional equality ##

We'll re-use the trick employed by `symbolicArgCount`{.scheme} above, of using
`bumpSymbols`{.scheme} to ensure all symbol indices are incremented, and hence
that `(V 0)`{.scheme} does not appear in its result. If *two* such expressions
are equal when applied to `(V 0)`{.scheme}, that must be due to the expressions
themselves, and not the value of `(V 0)`{.scheme} (which is uninterpreted, and
does not appear in the expressions). That proves they are equal for *any*
argument, and hence they are extensionally equal:

```{.scheme pipe="./show"}
(ruleset extensional)
(rule ((= (App (bumpSymbols x) (V 0))
          (App (bumpSymbols y) (V 0)))
       (= (symbolicArgCount x)
          (symbolicArgCount y)))
      ((union x y))
      :ruleset extensional)
```

The use of `symbolicArgCount`{.scheme} ensures that the underlying expressions
begin with a concrete part (since that's what we really care about), and that
they have the same arity. For arities greater than one, this rule will work
backwards recursively: using equality of their final result to make them equal
without the last argument; then using that equality to make them equal without
the second-to-last argument; and so on (working beneath another layer of
`bumpSymbols`{.scheme} each time, so those last arguments always match
`(V 0)`{.scheme}), until their concrete parts become equal.

## Testing ##
