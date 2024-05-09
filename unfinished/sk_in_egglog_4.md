---
title: "SK in egglog: part 4, extensional equality"
packages: [ 'egglog', 'graphviz', 'timeout' ]
---

[Part 1](/blog/2024-02-25-sk_logic_in_egglog_1.html)

[Part 2](/blog/2024-03-17-sk_logic_in_egglog_2.html)

[Part 3](/blog/2024-04-02-sk_logic_in_egglog_3.html)

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

This post continues my explorations with
[egglog](https://github.com/egraphs-good/egglog), using it to implement [SK
logic](https://news.ycombinator.com/item?id=22270327). Here's the code so far,
with some `ruleset` annotations sprinkled in to give us more control over what
to `run`:

```{.scheme pipe="./show"}
;; Applicative combinatory logic. We use (C "foo") for constants, which will
;; show up in the e-graph and output of egglog.
(datatype Com
          (App Com Com)
          (C String))

;; Base combinators S and K
(let S (C "S"))
(let K (C "K"))

;; Example combinator
(let I (App (App S K) K))

;; Rewrite rules for running SK expressions
(ruleset reduce)
(rewrite      (App (App K x) y)    x                         :ruleset reduce)
(rewrite (App (App (App S x) y) z) (App (App x z) (App y z)) :ruleset reduce)
```

### A note on infinite loops and recursion ###

Modelling a Turing-complete system like SK requires care, to reach the desired
results whilst avoiding infinite recursion. The SK reduction rules are the main
danger, since they will attempt to "run" every expression in the database, so we
need to avoid expressions which blow up.

Note that e-graphs are perfectly happy with infinite *cycles*, for example the
following expression `omega`{.scheme} will be reduced by the `S`{.scheme} rule,
but ends up back where it started after a few steps; that's normally considered
an "infinite loop", but egglog's cumulative approach keeps track of *all* the
states, and stops as soon as the pattern repeats a previous form:

```{.scheme pipe="./show"}
(let sii   (App (App S I) I))
(let omega (App sii sii))
```

```{.scheme pipe="./show omega.egg"}
;; A "saturate" schedule will keep running until the database stops changing
(run-schedule (saturate reduce))
(check (= omega (App (App I sii) (App I sii))))
```

<details class="odd">
  <summary>**Output…**</summary>

```{pipe="sh"}
set -e
./run omega.egg
```

</details>

## Representing symbols ##

The last couple of installments explored the idea of extensionality, using
Haskell to uncover a problem I was running into in egglog. The cause turned out
to be using symbolic inputs to check if expressions agreed, when those
expressions may have already contained those symbols. Now we're switching back
to egglog, our first task is to distinguish expressions that contain such
uninterpreted symbols.

We'll represent uninterpreted symbols numerically. One way to do this is using a
[unary/Peano-style encoding](https://en.wikipedia.org/wiki/Peano_axioms#Historic_second-order_formulation),
with a "zeroth symbol" and [a function for the "successor
symbol"](https://en.wikipedia.org/wiki/Successor_function). However, egglog has
numbers built-in via its `i64`{.scheme} `sort`{.scheme}, so we might as well use
those (they're also backed by efficient Rust code)! We'll turn these
`i64`{.scheme} values into `Com`{.scheme} values using an egglog
`function`{.scheme} called `V`{.scheme} (for Variable; since we're already using
`S`{.scheme} for something else):

```{.scheme pipe="./show"}
(function V (i64) Com)
```

We'll avoid defining any outputs for `V`{.scheme}, so it remains uninterpreted:
that way egglog can treat `(V 0)`{.scheme}, `(V 1)`{.scheme}, etc. as
`Com`{.scheme} values, but it cannot make any further assumptions about their
structure or relationships.

We'll still use the idea of a "successor symbol", but that can now be
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
<summary>**Aside: "printf debugging" in egglog…**</summary>

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

For example, populating the database with some seed values and running until
saturation:

```{.scheme pipe="./show bump-loud.egg"}
(bumpSymbols (App I (V 0)))
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

We'll explain the `:merge`{.scheme} clause in a moment (it's required to avoid
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

Larger counts occur when we apply an expression to a symbol which doesn't occur
in that expression. We can ensure this using `bumpSymbols`{.scheme}: since its
result has all its symbols incremented, we *know* that it cannot contain the
first symbol `(V 0)`{.scheme}:

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

There's an interesting subtlety here, since it's really *equivalence classes*
that are concrete, and those *may* contain symbolic values! For example,
consider the following expression:

```{.scheme pipe="./show count-redex.egg"}
(let expr (App K S))
```

We can `check` that this is concrete (i.e. unchanged by `bumpSymbols`{.scheme}),
and hence that its `symbolicArgCount`{.scheme} is `0`{.scheme}:

```{.scheme pipe="./show count-redex1.egg"}
(run-schedule (saturate reduce symbols))
(check (= (bumpSymbols expr) expr))
(extract (symbolicArgCount expr))
```

```{pipe="sh"}
set -e
cat count-redex.egg count-redex1.egg > count-test1.egg
./run count-test1.egg 2> >(tee count-test1.err 1>&2)
```

<details class="odd">
  <summary>**Stderr…**</summary>

```{pipe="sh"}
cat count-test1.err
```

</details>

So far so good. Now let's apply it to a symbolic argument, matching the second
`rule` for `symbolicArgCount`{.scheme}:

```{.scheme pipe="./show count-redex2.egg"}
(let symbolic (App (bumpSymbols expr) (V 0)))
```

According to that `rule`, we would expect `(symbolicArgCount symbolic)`{.scheme}
to be `(+ 1 (symbolicArgCount expr))`{.scheme}, and hence to return `1`:

```{.scheme pipe="./show count-redex2.egg"}
(run-schedule (saturate reduce symbols))
(extract (symbolicArgCount symbolic))
```

```{pipe="sh"}
set -e
cat count-redex.egg count-redex2.egg > count-test2.egg
./run count-test2.egg 2> >(tee count-test2.err 1>&2)
```

<details class="odd">
  <summary>**Stderr…**</summary>

```{pipe="sh"}
cat count-test2.err
```

</details>

Uh oh, we got `0` instead! Remember that `(bumpSymbols (App K S))`{.scheme} is
equal to `(App K S)`{.scheme} (since it's concrete), and the reduction rule for
`K`{.scheme} says that `(App (App K S) (V 0))`{.scheme} is equal to `S`. Since
egglog functions are applied to *equivalence classes*, rather than particular
terms, the value of `(symbolicArgCount symbolic)`{.scheme} must be the same as
`(symbolicArgCount S)`{.scheme}, and the latter is clearly `0`{.scheme}. We fix
this ambiguity using the `:merge` parameter in the definition of
`symbolicArgCount`{.scheme}: when equal inputs give unequal results, like
`0`{.scheme} and `1`{.scheme} in this case, the `:merge`{.scheme} expression is
used, with the conflicting values bound to the names `old`{.scheme} and
`new`{.scheme}. We use `min`{.scheme} to choose the smaller value (in this case
`0`{.scheme}), since that is a property of the *class*. Larger values are more
arbitrary, e.g. in this case the value `1`{.scheme} is due to matching an
expression that SK will ultimately *discard*, and hence seems less fundamental.

Now that we have a working definition of `symbolicArgCount`{.scheme}, we have
enough information to finally implement extensional equality!

## Extensional equality ##

We'll re-use the trick employed by the second `symbolicArgCount`{.scheme} rule,
that `(bumpSymbols foo)`{.scheme} is guaranteed to not contain `(V 0)`{.scheme}
(at least, in no place that can affect its result; it may still appear in the
equivalence classes due to reduction rules, like in the `symbolic`{.scheme}
example).

If *two* such expressions are equal when applied to `(V 0)`{.scheme}, their
definitions must be inherently equal *independent* of argument (since
`bumpSymbols` ensures independence from `(V 0)`{.scheme} *syntactically*; and
its behaviour as an inert, uninterpreted symbol ensures independence
*semantically*). That proves they are equal for *any* argument, and hence they
are extensionally equal:

```{.scheme pipe="./show"}
(ruleset extensional)
(rule ((= (App (bumpSymbols x) (V 0))
          (App (bumpSymbols y) (V 0)))
       (= (symbolicArgCount x)
          (symbolicArgCount y)))
      ((union x y))
      :ruleset extensional)
```

This `rule` also requires both expressions to have the same
`symbolicArgCount`{.scheme}, which ensures that the underlying expressions
begin with a concrete part (since that's what we really care about), and that
they have the same arity. For arities greater than one, this rule will work
backwards recursively: using equality with `n` arguments to assert equality with
`n-1` arguments, and so on until their concrete parts become equal.

## Ignored arguments ##

One way that two expressions can be extensionally equal is if they *ignore* the
only terms at which they differ. For example, the `K` reduction rule discards
its second argument; hence expressions which only differ in the second argument
to `K` will be equal, like `(App (App K (V 0)) (V 1))`{.scheme} and
`(App (App K (V 0)) (V 2))`{.scheme}. Of course, that doesn't require
extensionality, since such expressions are equal *due to* that reduction rule
(in this example, both expressions are equal to `(V 0)`{.scheme}).

Yet this simple analysis can be pushed a little further, if we note those
expressions which will discard their *next* argument. We can represent this with
an `ignoresArg`{.scheme} `relation`:

```{.scheme pipe="./show"}
(ruleset ignored)
(relation ignoresArg (Com))
```

This `relation` will be useful to augment the existing extensionality rules,
which are limited by their pre-conditions (in order to avoid infinite
recursion!). We've already seen one way that expressions can ignore their next
argument:

```{.scheme pipe="./show"}
(rule ((App K x))
      ((ignoresArg (App K x)))
      :ruleset ignored)
```

A more sophisticated form uses the `S`{.scheme} rule to delay the construction
of reducible expressions; waiting for some further argument to be applied.
Consider the following example:

```{.scheme pipe="./show simple.egg"}
;; Declare an unspecified, concrete Com value
(declare foo Com)
(union (bumpSymbols foo) foo)

;; Two distinct ways of reducing to foo when applied to another argument
(let kFoo1 (App K foo))
(let kFoo2 (App (App S (App K (App K foo))) I))
```

Neither of these expressions is reducible, but they *are* extensionally equal,
since they agree on one argument:

 - `(App kFoo1 (V 0))`{.scheme} reduces to `foo`{.scheme} via the `K` rule
 - `(App kFoo2 (V 0))`{.scheme} reduces to
   `(App (App (App K (App K foo)) (V 0)) (App I (V 0)))`{.scheme} via the `S`
   rule, then via the `K` rule to `(App (App K foo) (App I (V 0)))`{.scheme},
   and again to `foo`{.scheme}.

(In fact, the `I`{.scheme} argument at the end of `kFoo2`{.scheme} is *also*
ignored during reduction!) We can spot this behaviour by noting that if
`(ignoresArg (App f x))`{.scheme} for *all* `x`{.scheme}, then
`(ignoresArg (App S f))`{.scheme}. To see this, give the latter a couple more
arguments `(App (App (App S f) a) b)`{.scheme}, so it reduces to
`(App (App f b) (App a b))`: we're assuming that `(App f b)`{.scheme} ignores
its next argument, making `(App a b)`{.scheme} irrelevant. Since that's the only
occurence of `a`{.scheme}, its value is also ignored; and `a`{.scheme} is the
next argument of `(App S f)`{.scheme}. ∎

```{.scheme pipe="./show"}
(rule ((ignoresArg (App (bumpSymbols f) (V 0))))
      ((ignoresArg (App S f)))
      :ruleset ignored)

(rule ((ignoresArg (App f x))
       (App S f))
      ((App (bumpSymbols f) (V 0)))
      :ruleset ignored)
```

Ignoring the *third* argument of `S` is less common, since it only happens when
*both* of the first and second arguments ignore it:

```{.scheme pipe="./show"}
(rule ((ignoresArg x)
       (ignoresArg y)
       (App (App S x) y))
      ((ignoresArg (App (App S x) y)))
      :ruleset ignored)
```

Finally, we can tell egglog that all applications of an an expression which
`ignoresArg`{.scheme} are (extensionally) equal:

```{.scheme pipe="./show"}
(rule ((= x (App f a))
       (= y (App f b))
       (ignoresArg f))
      ((union x y))
      :ruleset ignored)
```

This is enough to make our example expressions equal:

```{.scheme pipe="./show simple.egg"}
(App kFoo2 (V 0))  ;; Apply kFoo2 to something, to satisfy pre-conditions
(run-schedule (repeat 8 (seq reduce symbols extensional ignored)))
(check (= kFoo1 kFoo2))
```

<details class="odd">
  <summary>**Output…**</summary>

```{pipe="sh"}
set -e
./run simple.egg
```

</details>

Another test for these rules is the identity combinator `I`{.scheme}, which we
defined as `(App (App S K) K)`{.scheme}. However, that second `K`{.scheme} is
ignored, so *any* other expression should be usable in its place:

```{.scheme pipe="./show i.egg"}
(let I1 (App (App S K) K))
(let I2 (App (App S K) S))
(let I3 (App (App S K) I))
(let I4 (App (App S K) foo))

(run-schedule (saturate reduce symbols extensional ignored))
(check
  (= I I1)
  (= I I2)
  (= I I3)
  (= I I4))
```

<details class="odd">
  <summary>**Output…**</summary>

```{pipe="sh"}
set -e
./run i.egg
```

</details>

## Boolean logic ##

Extensional equality extends beyond simply *ignoring* arguments: expressions
which *use* their arguments in equal ways are also extensionally equal. We'll
test this using the following [encoding of
boolean logic](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans),
where `(App (App TRUE x) y)`{.scheme} reduces to `x`{.scheme} and
`(App (App FALSE x) y)`{.scheme} reduces to `y`{.scheme}:

```{.scheme pipe="./show"}
(let TRUE  K)
(let FALSE (App S K))
```

[Boolean operations](https://en.wikipedia.org/wiki/Boolean_algebra#Operations),
such as `NOT`, `AND` and `OR`, can be encoded as SK expressions which, when
applied to such encoded booleans, reduce to one or the other of those encoded
booleans.

### NOT ###

Here's a definition of `NOT`:

```{.scheme pipe="./show notTrue.egg | ./show notFalse.egg"}
(let NOT (App (App S (App (App S (App K S))
                          (App (App S (App K K))
                               S)))
              (App K K)))
```

It's chosen such that its application to `TRUE` agrees with `FALSE`:

```{.scheme pipe="./show notTrue.egg"}
(let notTrue       (App NOT TRUE))
(let notTrueResult (App (App notTrue (V 1)) (V 0)))
(let   falseResult (App (App FALSE   (V 1)) (V 0)))
```

And its application to `FALSE` agrees with `TRUE`:

```{.scheme pipe="./show notFalse.egg"}
(let notFalse       (App NOT FALSE))
(let notFalseResult (App (App notFalse (V 1)) (V 0)))
(let     trueResult (App (App TRUE     (V 1)) (V 0)))
```

These expressions seem to cause our rules to diverge, so we'll only run them
10 times:

```{.scheme pipe="./show notTrue.egg | ./show notFalse.egg"}
(run-schedule (repeat 10 (seq reduce symbols extensional ignored)))
```

First we'll check that our rules are consistent:

```{.scheme pipe="./show notTrue.egg | ./show notFalse.egg"}
(check (!= TRUE FALSE))
```

Then we can check the behaviour of `notTrue` and `notFalse`:

```{.scheme pipe="./show notTrue.egg"}
(check
  (= falseResult (V 0))
  (= falseResult notTrueResult))
```

```{.scheme pipe="./show notFalse.egg"}
(check
  (= trueResult (V 1))
  (= trueResult notFalseResult))
```

Since these expressions agree on two inputs, our extensional equality rules
should make them equal:

```{.scheme pipe="./show notTrue.egg"}
(check (= FALSE notTrue))
```

```{.scheme pipe="./show notFalse.egg"}
(check (= TRUE notFalse))
```

<details class="odd">
  <summary>**Output…**</summary>

I've written the `notTrue`{.scheme} and `notFalse`{.scheme} code into separate
files. Here's the output from the `notTrue`{.scheme} examples:

```{pipe="sh"}
set -e
./run notTrue.egg
```

Here's the output for `notFalse`{.scheme}

```{pipe="sh"}
set -e
./run notFalse.egg
```

</details>

### AND ###

Here is `AND`:

```{.scheme pipe="./show and.egg"}
(let AND (App (App S S) (App K (App K FALSE))))
```

We can specify its behaviour on *four* arguments: if the first two are `TRUE`,
it returns the third (i.e. it equals `TRUE`); otherwise it returns the fourth
(equalling `FALSE`). Thanks to extensional equality, we can test this using only
values of the *first* argument:

```{.scheme pipe="./show and.egg"}
(let andTrue  (App AND TRUE ))
(let andFalse (App AND FALSE))

(App (App (App andFalse (V 2)) (V 1)) (V 0))
(App (App (App andTrue  (V 2)) (V 1)) (V 0))

(run-schedule (repeat 10 (seq reduce symbols extensional ignored)))
```

To see this, notice that the behaviour of `andFalse`{.scheme} does not depend on
its next argument: it will always act like `FALSE`, and hence
`andFalse`{.scheme} should equal `(App K FALSE)`{.scheme}:

```{.scheme pipe="./show and.egg"}
(check (= (App K FALSE) andFalse))
```

Likewise, the behaviour of `andTrue`{.scheme} is entirely determined by its next
argument: hence it should be equal to `I`:

```{.scheme pipe="./show and.egg"}
(check (= I andTrue))
```

<details class="odd">
  <summary>**Output…**</summary>

```{pipe="sh"}
set -e
./run and.egg
```

</details>

### OR ###

We can characterise `OR` in a similar way:

```{.scheme pipe="./show or.egg"}
(let OR (App (App S I) (App K TRUE)))

(let orTrue  (App OR  TRUE ))
(let orFalse (App OR  FALSE))

(App (App (App orTrue  (V 2)) (V 1)) (V 0))
(App (App (App orFalse (V 2)) (V 1)) (V 0))

(run-schedule (saturate reduce symbols extensional))
```

This time, `orTrue` ignores its next argument:

```{.scheme pipe="./show or.egg"}
(check (= (App K TRUE) orTrue))
```

Whilst `orFalse` depends entirely on its next argument:

```{.scheme pipe="./show or.egg"}
(check (= I orFalse))
```

<details class="odd">
  <summary>**Output…**</summary>

```{pipe="sh"}
set -e
./run or.egg
```

</details>

## Conclusion ##

I've finally achieved what I wanted from my first foray into egglog: an encoding
of SK combinatory logic which can automatically discover extensional equality.
It took a few attempts and a bit of head-scratching, to discover that our naïve
symbolic execution was making inconsistent inferences; and to come up with a
symbol-counting mechanism to avoid this.

Overall I'm quite happy, since I've learned a bit more about SK, symbolic
execution and the treatment of free variables. I've also learned *a lot* about
egglog, and this experience has sharpened some of the vague ideas I'd been
considering for it. Hopefully I can get something more "serious" working in the
not-too-distant future!
