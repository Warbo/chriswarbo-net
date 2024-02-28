---
title: "SK logic in egglog: part 1, encoding and reduction"
packages: [ 'egglog', 'graphviz' ]
---

```{pipe="cat > show && chmod +x show"}
#!/bin/sh
[[ "$#" -eq 1 ]] || {
  echo "${NAME:-show} needs a filename as argument" 1>&2
  exit 1
}
echo >> "$1"
tee -a "$1"
echo >> "$1"
```

```{pipe="cat > hide && chmod +x hide"}
#!/bin/sh
NAME=hide ./show "$@" > /dev/null
```

I've been really excited to try out
[egglog](https://github.com/egraphs-good/egglog), since it seems like a great
complement and application for the theory exploration systems I spent a few
years playing with.

Egglog can be thought of as a powerful database, combining Datalog (a restricted
form of Prolog, which is essentially first-order logic) and equality-saturation.
The latter is most interesting to me, since it represents expressions via a
graph of equivalence classes; very similar to the internal representation that
QuickSpec v1 uses to search for conjectures (v2 switched to generating
individual expressions on-the-fly, more like IsaCoSy; although much faster).
I've played with equational systems like
[Maude](https://en.wikipedia.org/wiki/Maude_system) before, but egglog seems
like a sweet-spot in the design space.

You can try egglog using
[their online sandbox](https://egraphs-good.github.io/egglog/), or you can use
the `egglog` attribute of Nixpkgs.

## A trivial example ##

Let's define a very simple datatype called `Expr`, containing just two sorts
value:

 - A constant value `C`
 - A function `Id` applied to some other `Expr` (like `(Id C)`{.scheme},
   `(Id (Id C))`{.scheme}, etc.)

This is actually just a Peano/Grassman encoding of the natural numbers, but I'm
using different names since we're going to deviate from that definition in a
moment. Instead, think of it as a very simple programming language, with two
sorts of expression.

### A simple encoding ###

The most direct way to represent this in egglog is the following:

```{.scheme pipe="./show id-simple.egg"}
(datatype Expr
    (Id Expr))
(declare C Expr)
```

Running this gives:

```{pipe="sh"}
egglog id-simple.egg 2>&1
#[INFO ] Declared sort Expr.
#[INFO ] Declared function Id.
#[INFO ] Declared function v0___.
```

The `datatype` keyword is a shorthand for declaring a `sort` (i.e. a type), and
for defining any number of constructors: `function`s which accept inputs and
return a value of this `sort`. Here we can see that `Id` is such a `function`.
Since `C` is a constant, rather than a `function`, we `declare` it separately;
however, this results in an auto-generated nullary function with the unhelpful
name `v0___`. That may be fine for many applications, but for these explorations
I'd like something more readable.

### An encoding with names ###

In order to preserve our constant's name, we'll embed it as a `String` inside
its value. To implement this we'll use an extra constructor `E`, giving us:

```{.scheme pipe="./show id-string.egg"}
(datatype Expr
    (Id Expr)
    (E  String))
```

Now our constant value can be represented using `(E "C")`{.scheme}, and we can
define a shorthand for it using egglog's `let`:

```{.scheme pipe="./show id-string.egg"}
(let C (E "C"))
```

Now we can tell when a value is our `C` constant, since it will contain the
string `"C"`{.python}. This can be seen if we ask egglog to draw its "database"
of known values, using the `--to-svg` argument:

```{pipe="cat > svg && chmod +x svg"}
egglog --to-svg "$1.egg" 1>&2
{
  printf '<img alt="%s" src="data:image/svg+xml;base64,' "$2"
  basenc -w0 --base64 < "$1.svg"
  printf '"/>'
} | pandoc -f html -t json
rm "$1.svg"
```

```{.unwrap pipe="sh"}
./svg id-string 'E-graph database containing constant C'
```

The inner box (labelled `E("C")`) is the constant value we defined using `let`.
The outer box is an *equivalence class*, whose contents are known to be equal to
each other. So far egglog only knows about our value `C`, so it's all alone in
the only equivalence class.

**Note:** Yes, it's annoying that the graphical output writes parentheses like
`E("C")`{.python}, whilst the actual egglog language uses prefix-style
`(E "C")`{.scheme}. That seems to be a quirk of the rendering; all of the actual
code will always use the latter style!

### How to run `Expr`essions ###

Next we'll tell egglog how to 'run' our language, by making `Id` act like an
identity function, i.e. returning the given `Expr` as its output. We can do that
using a `rewrite` rule, referring to the argument `Expr` using a variable `x`:

```{.scheme pipe="./show id-string.egg"}
(rewrite (Id x) x)
```

With this rule, wrapping an `Expr` value in any number of calls to `Id` results
in an equivalent `Expr`. Let's test this by defining a value that wraps `C` in
a few calls to `Id`, and `check` that it's still equal to `C`:

```{.scheme pipe="./show id-string.egg"}
(let thrice
    (Id (Id (Id C))))
```

```{.scheme pipe="sh"}
set -x
cp id-string.egg id-string-unrun-check.egg
echo '(check (= thrice C))' |
  tee id-check.egg |
  ./show id-string-unrun-check.egg
```

Running the above through egglog gives the following:

```{pipe="sh"}
set -x
egglog id-string-unrun-check.egg 2>&1 | tee >(cat 1>&2)
#[INFO ] Declared sort Expr.
#[INFO ] Declared function Id.
#[INFO ] Declared function v0___.
#[INFO ] Declared rule (rewrite (Id x) x).
#[ERROR] Check failed:
#    (= test C)
```

Uh oh, what went wrong?

#### Inspecting egglog ####

Let's show the value of `thrice`, using `(query-extract thrice)`{.scheme}:

```{pipe="sh"}
cp id-string.egg id-string-unrun-query.egg
echo '(query-extract thrice)' |
  tee id-query.egg |
  ./hide id-string-unrun-query.egg
egglog id-string-unrun-query.egg 2>&1
#[INFO ] extracted with cost 4: (Id (Id (Id (v0___))))
#(Id (Id (Id (v0___))))
```

This shows why our `test` failed: `thrice` still has three `Id` wrappers, so
egglog doesn't know it's equal to `C`. We'll see why our `rewrite` rule hasn't
been applied in the next section; first we'll take a look at egglog's database:

```{.unwrap pipe="sh"}
./svg id-string-unrun-query 'E-graph database with unreduced value thrice'
```

There are now multiple values and multiple (equivalence) classes. Those values
with arrows coming out are `function` *calls* (**not** the actual functions
themselves), with the arrows pointing to the inputs used by that call. For
example, the value pointing from `Id` to the class containing `E("C")`
represents the subexpression `(Id C)`{.scheme} in our `thrice` value; whilst the
value pointing to *that* (from another `Id` call) represents the subexpression
`(Id (Id C))`{.scheme}, and so on.

Notice that arrows point to a *class*, rather than a specific value. The class
containing `C` represents «anything equivalent to `C`», so the value
representing the call `(Id C)`{.scheme} is actually more general, representing
«`Id` applied to «anything equivalent to `C`»». That value lives in another
equivalence class, for «anything equivalent to «`Id` applied to «anything
equivalent to `C`»»». This builds up, so the value for `(Id (Id C))`{.scheme}
represents the more general «`Id` applied to «anything equivalent to «`Id`
applied to «anything equivalent to `C`»»»», and so on. The resulting "e-graph"
(equation graph) is a very compact way to store complex inter-relationships
between expressions.

#### Actually running `Expr`essions ####

The reason our `rewrite` rule didn't change anything is that we didn't tell
egglog to run! We do this by calling `run`, but we have to decide *how much* to
run: egglog rules can cause expressions to grow and grow, so we need to provide
a cutoff. Due to egglog's unification/canonicalisation mechanism, we'll see that
only a single application of our `rewrite` rule will be needed, which we can do
with `(run 1)`{.scheme}:

```{pipe="sh"}
echo '(run 1)' | ./show id-string.egg > id-run.egg
egglog id-string.egg 2>&1
# [INFO ] Declared sort Expr.
# [INFO ] Declared function Id.
# [INFO ] Declared function E.
# [INFO ] Declared rule (rewrite (Id x) x).
# [INFO ] Ran schedule (repeat 1 (run)).
# [INFO ] Report: Rule (rule ((= rewrite_va...: search 0.000s, apply 0.000s
#     Ruleset : search 0.000s, apply 0.000s, rebuild 0.000s
```

Now we can try our `check` and `query-extract` again:

```{pipe="sh"}
< id-check.egg ./hide id-string.egg
< id-query.egg ./hide id-string.egg
egglog id-string.egg 2>&1
# [INFO ] Declared sort Expr.
# [INFO ] Declared function Id.
# [INFO ] Declared function E.
# [INFO ] Declared rule (rewrite (Id x) x).
# [INFO ] Ran schedule (repeat 1 (run)).
# [INFO ] Report: Rule (rule ((= rewrite_va...: search 0.000s, apply 0.000s
#     Ruleset : search 0.000s, apply 0.000s, rebuild 0.000s
#
# [INFO ] Checked fact [ConstrainEq("thrice", "C")].
# [INFO ] extracted with cost 2: (E "C")
# (E "C")
```

Success! Now let's look at the database:

```{.unwrap pipe="sh"}
./svg id-string 'E-graph database with reduced expression'
```

Now that egglog has run our rewrite rule, it's discovered that *all* of the
previous values are equivalent. Furthermore, the number of values has collapsed
to just two: representing `C` and «`Id` applied to «anything equivalent to
`C`»». The previous represention of `(Id (Id C))`{.scheme} has unified with that
of `(Id C)`{.scheme}, since *both* of their inputs are now encompassed by the
same class «anything equivalent to `C`»; and likewise for
`(Id (Id (Id C)))`{.scheme}. In fact, that single `Id` call now represents *any*
number of wrappers around `C`, showing them all to be equivalent!

## A larger example: SK logic ##

Now we've seen the basics of egglog, we can use it for a *real* programming
language; specifically
[SK combinatory logic](https://news.ycombinator.com/item?id=22270327)! This time
we'll use a binary constructor `App`, to represent the application of one
combinator to another; and use the same `String` trick to represent two
constants `S` and `K`:

```{.scheme pipe="./show sk.egg"}
(datatype Com
    (App Com Com)
    (C String))
(let S (C "S"))
(let K (C "K"))
```

SK expressions are evaluated according to the following rules, which reduce a
`K` once it's applied to two expressions; and an `S` once it's applied to three:

```{.scheme pipe="./show sk.egg"}
(rewrite (App (App K x) y) x)
(rewrite (App (App (App S x) y) z) (App (App x z) (App y z)))
```

SK logic is a universal programming language, although it can be a bit unwieldy
to read and write due to the complete absence of any naming mechanism.
We'll use our `String`-embedding trick to define useful constants, and use a
`union` rule to unify their name with their SK definition.

For example, here is the identity function. Sometimes this is implemented using
its own rewrite rule (giving SKI logic), but that's redundant since the `S` and
`K` rules are all we need:

```{.scheme pipe="./show sk.egg"}
(let I (C "I"))
(union I (App (App S K) K))
```

Here's a Church-encoding of booleans and if/then/else:

```{.scheme pipe="./show sk.egg"}
(let TRUE  (C "TRUE"))
(let FALSE (C "FALSE"))
(let IF    (C "IF"))
(union TRUE  K)
(union FALSE (App S K))
(union IF    I)
```

We'll test these using the following expression:

```{pipe="./show sk.egg"}
(let test
    (App (App (App IF (App I FALSE)) S) (App K TRUE)))
```

```{pipe="sh"}
cp sk.egg sk-test1.egg
```

Let's walk through how we *expect* this to evaluate, assuming we've got the SK
definitions of each correct:

 - `(App I FALSE)`{.scheme} should equal `FALSE`, since `I` should act like the
   identity function.
 - `(App IF FALSE)`{.scheme} should accept two arguments and return the second,
   like the "else branch" in other languages.
 - In this case that "else branch" is `(App K TRUE)`{.scheme}
 - `(App K TRUE)`{.scheme} won't reduce any more (since it doesn't have enough
   arguments), so it should be the final result (or equivalent).

Before running anything, let's query the initial value of `test` and take a look
at the database:

```{pipe="sh"}
cp sk.egg sk-test.egg
echo '(query-extract test)' | ./hide sk-test.egg
egglog sk-test.egg 2>&1
```

```{.unwrap pipe="sh"}
./svg sk-test 'Initial e-graph database of SK values'
```

**Note:** Calls with multiple inputs are shown with multiple arrows emerging;
the order of the arrows from left-to-right matches the order of the inputs.

So far the only equivalences egglog knows are those constants we explicitly
defined with `union`. The value of `test` looks a little funny, since some of
those definitions overlap and egglog may pick different representatives from the
equivalence classes than the ones we originally wrote. The `rewrite` rules for
`S` and `K` haven't reduced anything yet. Let's see what happens after using
`(run 1)`{.scheme}:

```{pipe="sh"}
cp sk.egg sk-test1.egg
{
  echo '(run 1)'
  echo '(query-extract test)'
} | ./hide sk-test1.egg
egglog sk-test1.egg 2>&1
```

```{.unwrap pipe="sh"}
./svg sk-test1 'SK database after running for 1 step'
```

A little simplification has occurred, combining some of the `App` calls (e.g.
look at those which apply `I`), but the changes are not significant enough to
affect the value of `test`. Let's try again with `(run 2)`{.scheme}:

```{pipe="sh"}
cp sk.egg sk-test2.egg
{
  echo '(run 2)'
  echo '(query-extract test)'
} | ./hide sk-test2.egg
egglog sk-test2.egg 2>&1
```

```{.unwrap pipe="sh"}
./svg sk-test2 'SK database after running for 2 steps'
```

Again, some more simplification has occurred: this time the `IF` expression has
simplified, since egglog has discovered that `(App I FALSE)`{.scheme} and
`(App IF (App I FALSE))`{.scheme} are both equivalent to `FALSE`.

(Notice that this is similar to our `Expr` example, since `IF` is defined as
`I`, so `(App IF (App I FALSE))`{.scheme} is a bit like `(Id (Id C))`{.scheme})

Using `(run 3)`{.scheme} leads to further changes, but no direct effect on the
value of `test`:

```{pipe="sh"}
cp sk.egg sk-test3.egg
{
  echo '(run 3)'
  echo '(query-extract test)'
} | ./hide sk-test3.egg
egglog sk-test3.egg 2>&1
```

```{.unwrap pipe="sh"}
./svg sk-test3 'SK database after running for 3 steps'
```

Finally `(run 4)` is able to fully reduce `test`. The database seems to be
saturated now, since running for longer does not lead to any more changes:

```{pipe="sh"}
cp sk.egg sk-test4.egg
{
  echo '(run 4)'
  echo '(query-extract test)'
} | ./hide sk-test4.egg
egglog sk-test4.egg 2>&1
```

```{.unwrap pipe="sh"}
./svg sk-test4 'SK database after running for 4 steps'
```

### Beyond β-equivalence ###

Our rewrite rules for reducing `S` and `K` expressions correspond to the
β-reduction rule of λ-calculus, so we can say that an expressions like
`(App (App K (App S S)) S)`{.scheme} is *β-equivalent* to the expression
`(App S S)`{.scheme} (and vice-versa). There are other forms of equivalence we
might want to implement, although the α-equivalence and η-equivalence of
λ-calculus aren't directly applicable to SK logic (since it lacks names and
abstractions).

I am particularly interested in implementing *extensional equivalence*, which
holds for two expressions which return equal results when applied to any
argument; e.g. if `(App f x)`{.scheme} is equivalent to `(App g x)`{.scheme}
regardless of `x`, then `f` and `g` are extensionally equivalent. In the example
above, the expressions `(App FALSE K)`{.scheme} and `(App FALSE S)`{.scheme}
are extensionally-equivalent; in fact, *all* expressions of the form
`(App FALSE something)`{.scheme} are equivalent, since `FALSE` (defined as
`(App S K)`{.scheme}) will entirely ignore its first argument. Whilst
`(App FALSE K)`{.scheme} appears in the same equivalence class as `I`, egglog
hasn't put `(App FALSE S)`{.scheme} in that class.

Unfortunately my attempts to capture this in egglog so far have ended up
collapsing the entire SK language into one big equivalence class, which is not
particularly useful. Hence I've decided to punt on that for now, and stick a
"part 1" in the title. If I can get that working soon, I'll be sure to write a
part 2!
