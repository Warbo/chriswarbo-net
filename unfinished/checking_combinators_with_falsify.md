---
title: "SK logic in egglog: part 1½, checking assumptions with automated tests"
packages: [ 'ghcWithFalsify' ]
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

```{pipe="cat > run && chmod +x run"}
#!/bin/sh
set -eo pipefail
rm -f "$1"
cp Main.hs "$1"
echo >> "$1"

# Retry a few times, in case falsify random sampling fails to find a
# counterexample we wanted to show!
for RETRY in seq 1 10
do
  if GOT=$({
    echo '```haskell'
    tee -a "$1"
    printf '\n```\n\n```{pipe="sh"}\ncp root/%s ./\nrunhaskell %s%s\n```\n' \
      "$1" "$1" "$2"
  } | pandoc -f markdown -t json | panpipe)
  then
    echo "$GOT"
    exit 0
  fi
done
exit 1
```

```{pipe="cat > fail && chmod +x fail"}
#!/bin/sh
exec ./run "$@" ' 2>&1 && exit 1 || true'
```

In [part 1 I played around with the egglog
language/database](/blog/2024-02-25-sk_logic_in_egglog_1.html), when I started
getting results that were so unexpected and confusing that it made me question
some of the assumptions I'd been making about the system I was modelling: a
small programming language called
[SK (combinatory) logic](https://doisinkidney.com/posts/2020-10-17-ski.html).
I'm no stranger to being wrong about my code, but I had chosen SK since it's
*very* simple and *very* familiar: implementing SK is like writing
"hello world", something I've done countless times in all sorts of languages and
paradigms (hence why it was an obvious choice when I wanted to learn egglog).

It only took a few minutes to learn enough syntax to make a working interpreter,
and not much longer to try out some different types and representations (indeed,
a more complicated form of combinatory logic even appears in
[egglog's `examples/`
folder](https://github.com/egraphs-good/egglog/blob/main/tests/combinators.egg)!).
However, an innocuous-looking one-line extension brought the whole thing
crashing down: all structure vanished and every term collapsed into one big
blob, where everything equalled everything else (**note for non-logicians:**
That's [not good](https://en.wikipedia.org/wiki/Principle_of_explosion)!).

Despite several rewrites, various different encodings, and much scouring of
[egglog's Rust
source](https://github.com/egraphs-good/egglog/blob/main/src/ast/mod.rs), I kept
running into the same behaviour. This made me seriously doubt my understanding
of free variables. If I could spend years immersed in the worlds of mathematics,
computer science, post-graduate academia, and software development; yet fail to
grasp such a fundamental aspect in all that time; then what *else* might I be
wrong about? Am I a complete fraud?

Thankfully I also trained as a physicist, so I know what to do when theoretical
understanding goes awry: **experiment**! In software engineering we call our
theories "libraries", and the experiments which ground them in real-world
observations are called "tests". I decided to take a break from egglog to
thoroughly test the assumptions I was making; this time in the more familiar
context of Haskell, using my favourite hammer: [property-based
testing](https://increment.com/testing/in-praise-of-property-based-testing/).
This would also let me try out the new
[falsify framework](https://hackage.haskell.org/package/falsify), which
[I've blogged about before](/blog/2024-01-19-lazy_test_generators.html).

## The setup ##

I'll explain some of this jargon below, but in short: that problematic line of
code was my attempt to implement
[extensional equality](https://en.wikipedia.org/wiki/Extensionality); the idea
that if two things always behave the same then they are equal. To specify what
"always" means, we need some notion of [universal
quantification](https://en.wikipedia.org/wiki/Universal_quantification). That
doesn't quite fit into the formal system of egglog, so my idea was to
*approximate* universally-quantified variables using
[symbolic execution](https://en.wikipedia.org/wiki/Symbolic_execution). This
*should* be a conservative approach: never mistakenly claiming two things to be
equal, but also failing to spot many real equalities. Instead, it seemed to do
the extreme opposite: *always* claiming that *everything* is equal!

<details class="odd">
<summary>Preamble boilerplate...</summary>

This page defines the entire contents of a test script using
[active code](/projects/activecode). For more details, see [this page's Markdown
source](/git/chriswarbo-net/git/branches/master/unfinished/checking_combinators_with_falsify.html).

Here's the initial boilerplate for the script; it just needs GHC with `falsify`
in its package database:

```{.haskell pipe="./show Main.hs"}
module Main (main) where

import Control.Applicative ((<|>), liftA2)
import Control.Exception (assert)
import Control.Monad (guard)
import Data.Foldable
import Data.List (inits)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Natural (Natural, mkNatural)
import Test.Falsify.Generator (Gen, Tree(..))
import qualified Test.Falsify.Generator as Gen
import Test.Falsify.Interactive (shrink)
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as Predicate
import qualified Test.Falsify.Range as Range
```

</details>

## Combinatory logic ##

Combinatory logic (which I'll shamelessly abbreviate as "SK") is a particularly
simple programming language, which can be written entirely with two symbols,
traditionally called `S` and `K`, along with parentheses for grouping symbols
together. We'll represent SK expressions using the following Haskell datatype
which we call `Com`{.haskell} (this representation also matches my definition in
egglog):

```{.haskell pipe="./show Main.hs"}
data Com = C Char | App Com Com deriving (Eq, Ord)

left, right :: Com -> Maybe Com
left  (App x _) = Just x
left  _         = Nothing
right (App _ y) = Just y
right _         = Nothing
```

We use the `C`{.haskell} constructor to represent symbols, distinguished by a
`Char`{.haskell} value like `'S'`{.haskell} or `'K'`{.haskell}:

```{.haskell pipe="./show Main.hs"}
-- | Our "base" combinators. Haskell requires the initial letter of a value name
-- | to be lowercase.
s, k :: Com
s = C 'S'
k = C 'K'
```

We can group expressions together two at a time using `App`{.haskell}: this is a
little cumbersome, compared to e.g. implementing `s`{.haskell} and `k`{.haskell}
as Haskell functions, but it gives us more control over their evaluation (e.g.
to bound their recurive calls).

The `deriving`{.haskell} clause asks Haskell to automatically implement some
useful interfaces:

 - `Eq`{.haskell} provides the `==`{.haskell} function. The auto-generated
   implementation will check whether two `Com`{.haskell} values are identical
   (same structure and same `Char`{.haskell} data).
 - `Ord`{.haskell} provides comparisons like `<`{.haskell}. Haskell will
   generate a lexicographic implementation for us, but the details aren't
   important. Having *some* implementation of `Ord`{.haskell} lets us use
   efficient `Set`{.haskell} datastructures.

We'll implement the `Show`{.haskell} interface ourselves, for more compact
output:

```{.haskell pipe="./show Main.hs"}
instance Show Com where
  show (C c)             = [c]
  show (App x (App y z)) = show x ++ "(" ++ show (App y z) ++ ")"
  show (App x y)         = show x ++ show y
```

### Anatomy of SK expressions ###

We can refer to various parts of an SK expression using the following
terminology:

 - The "root" is the top-most constructor.
 - The "head" is the left-most leaf. If the expression itself is a leaf, like
   `S` or `K`, then that expression is its own head (and root!).
 - The "spine" is the chain of left-nested `App` constructors between the head
   and the root. If the expression is a leaf, its spine is empty.
 - The "arguments" (or "args") of that head are the right-hand children hanging
   off the spine, ordered from the head to the root. If the spine is empty,
   there are no arguments.

<figure>

```
          𝔸𝕡𝕡
         ╔═╝─┐
        App  S
     ╔═══╝───┐
    App     App
   ╔═╝─┐   ┌─┴─┐
   Ⓢ   K  App  S
         ┌─┴─┐
         K   K
```

<figcaption>Structure of the expression `SK(KKS)S`, represented by the Haskell
value `App (App (App s k) (App (App k k) s)) s`{.haskell}. The root and spine
are double-struck; the head is shown as `Ⓢ`, and its arguments are `K`, `KKS`
and `S`.
</figure>

The figure above shows the expression `SK(KKS)S` rendered as a tree. Its head is
`S`, and args are `K`, `KKS` and `S`. Each of those args can be broken down in
the same way: the head of `K` is `K` and it has no args; the head of `S` is `S`,
and it has no args; the head of `KKS` is `K` and it has args `K` and `S`, which
can again be broken down; and so on. We will sometimes describe an expression as
"applying 〈its head〉 to 〈its arguments〉".

### Running SK expressions ###

We can "run" SK expressions using two rewriting rules:

 - An expression applying `K` to two args can be replaced by the first arg.
 - An expression applying `S` to three args can be replaced by an expression
   which applies the first arg to the third arg and «the second applied to the
   third».

We can implement these as Haskell functions, returning `Nothing`{.haskell} if
the rule didn't match:

```{.haskell pipe="./show Main.hs"}
-- | Replaces Kxy with x, otherwise Nothing
stepK :: Com -> Maybe Com
stepK (App (App (C 'K') x) _) = Just x
stepK _ = Nothing

-- | Replaces Sxyz with xz(yz), otherwise Nothing
stepS :: Com -> Maybe Com
stepS (App (App (App (C 'S') x) y) z) = Just (App (App x z) (App y z))
stepS _ = Nothing
```

Notice that arguments can be *any* `Com`{.haskell} value, represented using the
Haskell variables `x`{.haskell}, `y`{.haskell} and `z`{.haskell}; but only
`App`{.haskell}, `s`{.haskell} and `k`{.haskell} have any effect on the output
(this will be important for symbolic execution later!).

Remarkably, these two rules make SK a Turing-complete, universal programming
language! The following `step` function tries to apply both of these rules. It
also applies the rules recursively to sub-expressions of `App`{.haskell}: note
that we try stepping both children at once, to prevent an reducible expressions
getting "stuck" waiting for their sibling to finish. `Nothing`{.haskell} is
returned when neither rule matched and no sub-expressions were rewritten; we say
the expression is in [normal
form](https://en.wikipedia.org/wiki/Normal_form_(abstract_rewriting)):

```{.haskell pipe="./show Main.hs"}
-- | Attempt to reduce a K or S combinator, or the children of an App. Nothing
-- | if the argument is in normal form.
step :: Com -> Maybe Com
step c = stepK c <|> stepS c <|> a l' r' <|> a l' r <|> a l  r'
  where a        = liftA2 App
        (l , r ) = (left c    , right c   )
        (l', r') = (l >>= step, r >>= step)
```

Next we'll need to *iterate* the `step`{.haskell} function until its argument
reaches normal form. We'll distinguish normalised expressions using the type
`Normal`{.haskell} (we could encapsulate this in a separate module to prevent
unnormalised `Com`{.haskell} values being wrapped in `Normal`{.haskell}, but
I'll be sticking to one big module today):

```{.haskell pipe="./show Main.hs"}
-- | Wraps Com values which are assumed to be in normal form
newtype Normal = Normal { toCom :: Com } deriving (Eq, Ord)

instance Show Normal where
  show = show . toCom

-- | Wraps the argument in Normal iff it is in normal form
asNormal :: Com -> Maybe Normal
asNormal c = maybe (Just (Normal c)) (const Nothing) (step c)
```

Iterating `step`{.haskell} is tricky since SK is a universal programming
language, so we have to account for infinite loops, long-running computations,
exponential memory usage, etc. We'll do this by parameterising various functions
with `Fuel`{.haskell}: a natural number argument which decreases as we progress
through a computation; when it hits zero, we bail out:

```{.haskell pipe="./show Main.hs"}
 type Fuel = Word

 -- | Step the given Com until it reaches normal form. Gives Nothing if more than
 -- | n steps are required.
reduceN :: Fuel -> Com -> Maybe Normal
reduceN n c = case step c of
  Nothing  -> asNormal c
  Just c'  -> guard (n > 0) *> reduceN (n - 1) c'
```

### Normal equivalence ###

The rules of SK are
[confluent](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting)),
meaning that every expression has at most one `Normal`{.haskell} form, and
applying the reduction rules in any order, to any parts of an expression, will
not change its `Normal`{.haskell} form or the ability to reach it using those
rules. As a consequence, all SK expressions with the same `Normal`{.haskell}
form are equivalent and interchangable.

To see this, consider two expressions $X$ and $Y$ with the same
`Normal`{.haskell} form $N$. Whenever $X$ occurs inside a larger expression,
confluence allows us to replace $X$ with $N$ without affecting the overall
result. Now imagine we instead replace $X$ with $Y$: whatever that gives us, it
will be unaffected (thanks to confluence) if we reduce $Y$ to its
`Normal`{.haskell} form. Since the `Normal`{.haskell} form of $Y$ is also $N$,
and we already deduced that using $N$ is equivalent to using $X$, the result of
using $Y$ must *also* be equivalent to using $X$. Hence any expressions with the
same `Normal`{.haskell} form are equivalent, in the sense that they can be
interchanged inside any expression without altering its `Normal`{.haskell} form.

We'll call this relationship "`Normal`{.haskell} equivalence". Whilst it's
undecidable in general, we can approximate it using a `Fuel`{.haskell}
parameter:

```{.haskell pipe="./show Main.hs"}
-- | Result of comparing two values (which may be the same)
data Compared a = Same a | Diff a a

same (Same _)   = True
same _          = False
diff (Diff _ _) = True
diff _          = False

-- | Uses (==) to find identical values
comparer :: Eq a => a -> a -> Compared a
comparer x y = if x == y then Same x else Diff x y

-- | Try normalising the given Com values for the given number of steps. If the
-- | results are == return it in Left; otherwise return both in Right.
normalEqN :: Fuel -> Com -> Com -> Maybe (Compared Normal)
normalEqN n x y = comparer <$> reduceN n x <*> reduceN n y
```

## Property-based testing ##

Let's test this `normalEqN`{.haskell} function, to see whether it actually behaves
in the way our theory of SK predicts it should. For example, every equality
relation should have the
["reflexive property"](https://en.wikipedia.org/wiki/Reflexive_relation),
meaning that values should be equal to themselves. The following predicate
(function returning a boolean) checks whether its argument is `normalEqN`{.haskell}
to itself:

```{.haskell pipe="./show Main.hs"}
normalEqNToItself :: (Fuel, Com) -> Bool
normalEqNToItself (n, x) = maybe False same (normalEqN n x x)
```

We can turn this predicate into a general statement by *quantifying* its
argument. It's common to use [existential
quantification](https://en.wikipedia.org/wiki/Existential_quantification), which
asserts that *some* argument satisfies `normalEqNToItself`{.haskell}. This is the
widely practiced "example-based" approach to automated testing. For example:

```{.unwrap pipe="./run kIsNormalEqNToItself"}
main = assert (normalEqNToItself (0, k)) (putStrLn "PASS")
```

However, that isn't really what we want to say: reflexivity doesn't just apply
to a few hand-picked examples, it means that *every argument* satisfies
`normalEqNToItself`{.haskell}. Talking about "every argument" is [universal
quantification](https://en.wikipedia.org/wiki/Universal_quantification).
Universally-quantified assertions are called "properties", so this approach is
known as property-based testing.

To test a property, we give it to a "property checker" which searches for
*counterexamples*: argument values which cause the assertion to fail. If no
counterexample can be found, the test passes. Search techniques range from
[simple enumeration](https://hackage.haskell.org/package/smallcheck) all the way
up to [sophisticated AI
algorithms](https://en.wikipedia.org/wiki/American_Fuzzy_Lop_(software)).
Haskell has many property checkers, beginning with the wonderful
[QuickCheck package](https://hackage.haskell.org/package/QuickCheck). We'll use
the state-of-the-art for 2024, which is
[falsify](https://hackage.haskell.org/package/falsify).

### Data generators ###

`falsify` searches through argument values *at random*, sampling them from a
given "generator" with the Haskell type `Gen a`{.haskell} (for generating values
of some type `a`{.haskell}). We can build up generators using familiar
interfaces like `Applicative`{.haskell}, `Monad`{.haskell}, etc.

```{.haskell pipe="./show Main.hs"}
-- Combine individual results into a single tuple result. Works for any
-- Applicative, including Gen, Maybe, etc.

pair :: Applicative f => f a -> f b -> f (a, b)
pair a b = (,) <$> a <*> b

triple :: Applicative f => f a -> f b -> f c -> f (a, b, c)
triple a b c = (,,) <$> a <*> b <*> c

quad :: Applicative f => f a -> f b -> f c -> f d -> f (a, b, c, d)
quad a b c d = (,,,) <$> a <*> b <*> c <*> d
```

`falsify` also provides useful primitives to get started, such as
`Gen.inRange`{.haskell} which samples fixnums from a `Range`{.haskell}. This is
perfect for generating `Fuel`{.haskell}:

```{.haskell pipe="./show Main.hs"}
-- | Generates up to a certain amount of Fuel
genFuelN :: Fuel -> Gen Fuel
genFuelN max = Gen.inRange (Range.between (0, max))

-- | A reasonable default for procedures requiring Fuel. Small enough to keep
-- | exponentially-growing terms from blowing up.
limit :: Fuel
limit = 20

-- | Generates a (relatively small) amount of Fuel
genFuel = genFuelN limit
```

Generating `Com`{.haskell} values is more tricky, since they are recursive. A
naïve generator that simply calls itself recursively will make `Com`{.haskell}
values of *exponential* size: either blowing up memory (if the recursive case
`App`{.haskell} is likely to be chosen) or being limited to a handful of tiny
values (if the recursive case is unlikely to be chosen). To generate a diverse
spread of values, without risking out-of-memory conditions, we can use the same
`Fuel`{.haskell} trick as above; this time dividing it up (unevenly) between
recursive calls.

This is such a useful approach that I've implemented it for `QuickCheck`,
`ScalaCheck`, `Hypothesis`, etc. over the years. In contrast, `falsify` provides
it out-of-the-box! The `Gen.tree`{.haskell} function generates binary
`Tree`{.haskell} values of a given size, so we just need to transform those into
the `Com`{.haskell} values we need:

```{.haskell pipe="./show Main.hs"}
-- | Create a Com from a binary Tree (with unit values () at the nodes)
treeToCom :: Tree () -> Com
treeToCom t = case t of
  -- The smallest Trees have one and two leaves. Turn them into K, since that
  -- has the simplest step rule.
  Leaf               -> k
  Branch _ Leaf Leaf -> k

  -- There are two Trees with three leaves. Turn those into S.
  Branch _ Leaf (Branch _ Leaf Leaf) -> s
  Branch _ (Branch _ Leaf Leaf) Leaf -> s

  -- For larger Trees, we recurse on their children and combine using App
  Branch _ l r -> App (treeToCom l) (treeToCom r)

-- | Generate a Com, with size bounded by the given Fuel
genComN :: Fuel -> Gen Com
genComN n = treeToCom <$> Gen.tree (Range.between (0, n)) (pure ())

-- | Generate (relatively small) Com values
genCom = genFuel >>= genComN
```

Normally we would plug `falsify` generators into properties using Haskell's
`tasty` test framework, but we'll be a bit more direct with the following
function:

```{.haskell pipe="./show Main.hs"}
-- | Prints "PASS" if 'prop' holds for values sampled from 'gen'; otherwise
-- | prints a counterexample and fails.
run :: Show a => (a -> Bool) -> Gen a -> IO ()
run prop gen = shrink prop gen >>= maybe (putStrLn "PASS") abort
  where abort counterexample = fail ("FAIL: " ++ show counterexample)
```

### Included middles ###

The observant among you may have noticed that `normalEqNToItself`{.haskell} **does
not** hold for all argument values! `falsify` can generate a counterexample to
show us why:

```{.unwrap pipe="./fail normalEqNToItself"}
main = run normalEqNToItself (pair genFuel genCom)
```

The precise counterexample `falsify` finds may vary depending on the random
seed, but they'll all have the following in common: the `Fuel`{.haskell} will be
`0`{.haskell}, whilst the `Com` expression will not be in normal form. For
example, it may produce `KKK` (the Haskell value `App (App k k) k`{.haskell}).
`stepK`{.haskell} will reduce that to a single `K`, so whilst the value
`KKK` *is* equal to itself (as we expect), `normalEqN`{.haskell} will fail to show
this for *some* `Fuel`{.haskell} parameters: in particular, when given
`0`{.haskell} `Fuel`{.haskell}. This disproves our claim that *any* amount of
`Fuel`{.haskell} will work.

We could alter our claim to *existentially* quantify the amount of
`Fuel`{.haskell}: that for any SK expression `x`{.haskell}, there is *some*
value of `n`{.haskell} where `normalEqN n x x` holds. However, that is also false,
since there are infinite loops which have no normal form, regardless of how much
`Fuel`{.haskell} we use. When a normal form *does* exist, there is no way to
bound [the amount of `Fuel`{.haskell} required to find
it](https://en.wikipedia.org/wiki/Busy_beaver).

The correct way to fix our claim is to universally quantify `x`{.haskell} and
`n`{.haskell} as before, but add two negations to our predicate: instead of
claiming every expression is equal to itself (regardless of `Fuel`{.haskell}),
we claim that every expression is *not unequal* to itself (regardless of
`Fuel`{.haskell}):

```{.haskell pipe="./show Main.hs"}
notUnnormalEqNToItself :: (Fuel, Com) -> Bool
notUnnormalEqNToItself (n, x) = not (maybe False diff (normalEqN n x x))
```

```{.unwrap pipe="./run notUnnormalEqNToItself"}
main = run notUnnormalEqNToItself (pair genFuel genCom)
```

You may have learned in school that [double-negatives are
redundant](https://en.wikipedia.org/wiki/Double_negation_elimination): that
anything "not false" must be "true" (the
[law of the excluded middle](https://en.wikipedia.org/wiki/Law_of_excluded_middle)).
However, we're not dealing with the true/false ideals of
[classical logic](https://en.wikipedia.org/wiki/Classical_logic), which ignore
computation and its associated undecidability. All we have are the more
pragmatic falsified/unfalsified results of a property checker, where there are
non-excluded middles such as "don't know", "timed out" and "gave up"!

### Smarter generators ###

It's usually a good idea to write simple, straightforward properties like
`notUnnormalEqNToItself`{.haskell}, which make strong claims over a broad
space of input values. Sometimes it's a good idea to *also* have more specific
properties tailored to important cases. For example, the predicate
`normalEqNToItself`{.haskell} is actually `True`{.haskell} for the
`Normal`{.haskell} subset of `Com`{.haskell}:

```{.haskell pipe="./show Main.hs"}
normalNormalEqNToItself :: (Fuel, Normal) -> Bool
normalNormalEqNToItself (n, x) = normalEqNToItself (n, toCom x)
```

Checking this requires a generator which only produces `Normal`{.haskell}
values. That's actually much easier than our attempts to normalise values
*inside* a property, since generators are free to discard problematic values and
try again!

```{.haskell pipe="./show Main.hs"}
-- | Like genComN, but reduces its outputs to normal form. The fuel
-- | bounds the size of the initial expression (before it's reduced), and
-- | the number of steps to attempt when normalising.
genNormalN :: Fuel -> Gen Normal
genNormalN fuel = do
  c <- genComN fuel        -- Generate a Com value c
  maybe (genNormalN fuel)  -- Retry if c didn't finish reducing
        return             -- Return the Normal value if reduction finished
        (reduceN fuel c)   -- Try to reduce c to a Normal value

-- | Generates (relatively small) Normal values
genNormal :: Gen Normal
genNormal = genFuel >>= genNormalN
```

```{.unwrap pipe="./run normalNormalEqNToItself"}
main = run normalNormalEqNToItself (pair genFuel genNormal)
```

## Extensional equality ##

My problems arose when trying to extend the equality of SK expressions even
further to include
[extensional equality](https://en.wikipedia.org/wiki/Extensionality).

### Inputs and agreement ###

To clarify what we're about to do, we'll start by defining some more
terminology. An "input value" is some particular SK value, which another
expression will be applied to. For example, applying the expression `KSK` to the
input value `S` results in `KSKS`. Input values are hence just the right-most
arguments of such results; but it's useful to give them a more specific name,
since they are precisely those arguments in the result which are *separate* from
the first expression. Note that in this example, the result has head `K` and
arguments `S`, `K` and `S`; yet only the last of those was an input value.

```{.haskell pipe="./show Main.hs"}
-- Handy synonyms, to clarify the role of certain expressions

type InputValue  = Com
type InputValues = [InputValue]
```

If we apply two expressions to the same input value, and the results reduce to
the same `Normal`{.haskell} form, we say those two expressions "agree on" that
input value. For example, `KK` and `SKK` agree on the input value `K`, since
`KKK` reduces to `K`; and `SKKK` reduces to `KK(KK)` then to `K`. Note that they
*disagree* on the input value `S`, since `KKS` reduces to `K`; whilst `SKKS`
reduces to `KS(KS)` then to `S`. Expressions can also agree on a *sequence* of
input values, where we begin by applying them both to the first value, then
apply those results to the second value, and so on:

```{.haskell pipe="./show Main.hs"}
-- | Apply the Coms to the InputValues, see if they reach the same Normal form
agreeN :: Fuel -> Com -> Com -> InputValues -> Maybe (Compared Normal)
agreeN n f g (iv:ivs) = agreeN n (App f iv) (App g iv) ivs  -- Apply 1 & recurse
agreeN n f g []       = normalEqN n f g                     -- No more IVs, test
```

Everything that satisfies `normalEqN`{.haskell} should also satisfy
`agreeN`{.haskell}, which we can state with the following property:

```{.haskell pipe="./show Main.hs"}
normalEqNImpliesAgreeN :: (Fuel, Com, Com, InputValues) -> Bool
normalEqNImpliesAgreeN (n, f, g, xs) =
  case pair (normalEqN n f g) (agreeN n f g xs) of
    Just (Same _, Diff _ _) -> False
    _                       -> True
```

```{.unwrap pipe="./run normalEqNImpliesAgreeN"}
main = run normalEqNImpliesAgreeN (quad genFuel genCom genCom genComs)
```

An "input" is a universally-quantified input value, i.e. it can be *any* SK
expression, rather than one in particular. When expressions "agree on $N$
inputs", it means that applying them to *any* sequence of $N$ input values will
produce results that have the same `Normal`{.haskell} form. For example, `SK`
and `S(K(SK))(KK)` agree on two inputs; which we can test by asserting that they
*never disagree*:

```{.haskell pipe="./show Main.hs"}
skNeverDisagreesWithSKSKKK :: (Fuel, InputValue, InputValue) -> Bool
skNeverDisagreesWithSKSKKK (n, x, y) =
    not (maybe False diff (agreeN n f g [x, y]))
  where f = App s k
        g = App (App s (App k (App s k))) (App k k)
```

```{.unwrap pipe="./run skNeverDisagreesWithSKSKKK"}
main = run skNeverDisagreesWithSKSKKK (triple genFuel genCom genCom)
```

Note that any expressions which agree on $N$ inputs also agree on $N+1$ inputs
(and so on), since the left child of each root has the same `Normal`{.haskell}
form (by definition of agreement on $N$ inputs).

```{.haskell pipe="./show Main.hs"}
-- | Generate a list of Com values, with length and element size bounded by Fuel
genComsN :: Fuel -> Gen [Com]
genComsN fuel = do max <- genFuelN fuel
                   Gen.list (Range.between (0, max)) (genComN fuel)

-- | Generate (relatively small) lists of Com values
genComs :: Gen InputValues
genComs = genFuel >>= genComsN

-- | False iff the Com values agree on xs but not xs++ys
agreementIsMonotonic :: (Fuel, (Com, Com), (InputValues, InputValues)) -> Bool
agreementIsMonotonic (n, (f, g), (xs, ys)) =
  case pair (agreeN n f g xs) (agreeN n f g (xs ++ ys)) of
    Just (Same _, Diff _ _) -> False
    _                       -> True
```

```{.unwrap pipe="./run agreementIsMonotonic"}
main = run agreementIsMonotonic (triple genFuel
                                        (pair genCom  genCom )
                                        (pair genComs genComs))
```

### Extensionality ###

Now we've defined inputs and agreement, extensional equality becomes quite
simple: SK expressions are extensionally equal iff they agree on *some* number
of inputs. We saw that `SK` and `S(K(SK))(KK)` agree on *some* number of inputs
(namely: on two inputs), so they are extensionally equal.

Notice that the *number* of inputs is existentially-quantified, whilst the
*values* of those inputs are universally-quantified. This makes extensional
equality difficult to determine:

 - If we compare two expressions on $N$ input values and see that they *agree*,
   that doesn't mean they're extensionally equal; since there might be other
   input values of length $N$ for which they disagree.
 - If we compare two expressions on $N$ input values and see that they
   *disagree*, that doesn't mean they're *not* extensionally equal; since they
   might only agree on inputs longer than $N$.

In order to make more definitive assertions about extensional equality, we need
to borrow techniques from the world of formal methods!

## Symbolic execution ##

The reason I like property-checking is that it can provide a lot of confidence
for relatively little effort, compared to e.g. example-based testing (less
confidence for a similar effort), manual testing (less confidence for more
effort) or formal verification (more confidence for more effort). However, there
are occasions when the scales tip in favour of other approaches, and extensional
equality checking turns out to be particularly suited to a formal method called
[symbolic execution](https://en.wikipedia.org/wiki/Symbolic_execution).

In this approach, we avoid using real, "concrete" SK expressions for our input
values; and instead use
[abstract symbols](https://en.wikipedia.org/wiki/Symbol_(formal)). These can be
copied, discarded or rearranged during program execution; which is perfect for
seeing where different inputs end up in the resulting `Normal`{.haskell} form.
They are otherwise inert or "uninterpreted": causing reduction to stop if it
would depend on the particular value of an input.

Implementing symbolic execution can often be laborious, but our definition of
`Com`{.haskell} actually makes it trivial to graft on top: we can represent
abstract symbols using the `C`{.haskell} constructor applied to any
`Char`{.haskell} (*except* `'S'`{.haskell} or `'K'`{.haskell}, which we're
treating specially). Execution of the `stepK`{.haskell} and `stepS`{.haskell}
functions will treat these as
[atomic symbols](https://en.wikipedia.org/wiki/Symbol_(programming)), to be
copied, discarded and rearranged as needed.

### Interpreting symbolic expressions ###

We can now give a meaning, semantics, or [abstract
interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation) to SK
expressions containing symbols. Since we're using each symbol to represent a
distinct input, the way they appear in `Normal`{.haskell} forms tells us
something about the overall behaviour of the expression and, crucially, how it
may depend on the particular `Inputs`{.haskell} it gets applied to. There's no
way, in general, to decide whether part of an SK expression will influence its
reduction, or whether it will end up being discarded by the `stepK`{.haskell}
rule. Instead, we'll limit our ambitions to spotting a few simple situations.

```{.unwrap pipe="./run extensionallyEqNImpliesAgreeN"}
main = run extensionallyEqNImpliesAgreeN (triple genCom genCom genComs)
```

### A simplistic first attempt ###

We can now *directly* test our assumption that equal results on a symbolic input
imply equal results on all inputs, like like this:

```{.haskell pipe="./show Main.hs"}
-- | Asserts that the first two Com values give equal results when applied to
-- | the third. First argument limits the number of steps attempted.
assertEqualOn n f g x = case equalN n fx gx of
  (Just (Right (fx', gx'))) -> testFailed
    (concat [show fx, " -> ", show fx', "\n", show gx, " -> ", show gx'])
  _ -> pure ()
  where fx = App f x
        gx = App g x

-- | Generate two distinct Com values which normalise the same way when applied
-- | to a symbolic variable: check that they agree for a third Com value too.
prop_simplisticTest = do
    -- Generate a couple of Com values
    f <- gen genCom
    g <- gen genCom
    case equalN steps (App f v) (App g v) of
      -- When f and g are distinct and agree for v, try them on another input
      Just (Left _) | f /= g -> do
        x <- gen genCom
        assertEqualOn steps f g x
      -- When f and g disagree, discard them and try again
      _ -> discard
  where steps = 100
```

Whilst this test is *logically* correct, it's not very good. In particular it's
both *flaky* and, when it does work, it doesn't give us much *confidence*. These
problems come from the underlying statistics, and are evident in output like
`19 successful tests (discarded 521)`:

 - It's quite unlikely to generate suitable values for `f`{.haskell} and
   `g`{.haskell}, which just-so-happen to reduce `v`{.haskell} to the same
   value. Hence all of those discarded runs. The test harness will give up if
   too many runs are discarded, which makes the test flaky.
 - When we *do* generate a pair of suitable values, they are often ones that
   were already tested in a prior run. For example, using `collect`{.haskell} to
   gather statistics about suitable values shows them to be `K` 16% of the time
   and `S` 11% of the time (this varies between test invocations). This wastes
   resources and makes the "successful tests" number misleadingly inflated.
 - When we *do* happen to generate a successful pair of values, we're only
   checking them on a *single* argument, which isn't very thorough.

### A more sophisticated test ###

We're going to come up with a much better test, by applying a few simple ideas.
The result will be a bit more complicated, but more reliable and (crucially)
subject our assumption to a *much* more thorough barrage of checks.

#### Smarter generators to avoid discarding ####

The pattern-match in `prop_simplisticTest`{.haskell} will `discard`{.haskell}
the test unless very specific requirements are met. One is that
`f /= g`{.haskell} (i.e. `f`{.haskell} and `g`{.haskell} should not be
equal). Discarding an entire test run is a pretty wasteful way of ensuring this
precondition: especially since we *already have* the value of `f`{.haskell} when
we generate `g`{.haskell}! Here's a smarter generator, which keeps retrying
until it makes a value that's different from the given argument:

```{.haskell pipe="./show Main.hs"}
-- | Generate a Com value that is not equal to the given argument.
genUnequal :: Com -> Gen Com
genUnequal x = do
  c <- genCom
  if c == x
    then genUnequal x
    else pure c
```

This won't prevent *most* of the discarding in this case, but it's a good
technique to be aware of when writing property-based tests.

#### Generate *collections* to increase chance of collisions ####

The real problem is that a pair of values `f`{.haskell} and `g`{.haskell} are
very unlikely to be related in the way we need (that they reduce the input
`v`{.haskell} to the same value).  However, the structure of our test is quite
wasteful of the opportunities it has: when a test run is discarded, we *also*
discard the generated values of `f`{.haskell} and `g`{.haskell} values; let's
call them `f1`{.haskell} and `g1`{.haskell}. On the next iteration we generate a
fresh pair, say `f2`{.haskell} and `g2`{.haskell}; but we only compare those new
values to each other, and ignore the discarded values.

What if we *also* compared `f2`{.haskell} with `f1`{.haskell}, and
`f2`{.haskell} with `g1`{.haskell}, and `g2`{.haskell} with `f1`{.haskell} and
`g2`{.haskell} with `g1`{.haskell}? There are a total of *six* possible
relationships between the values we've generated; but we're currently only
checking ⅓ of them.  If we also discard that second pair and generate a fresh
`f3`{.haskell} and `g3`{.haskell}, that's *fifteen* possible relationships
between the values we've generated, of which we're only checking ⅕. Indeed, as
we generate more and more values the number of possible pairings follows
[sequence A000217 of the OEIS](https://oeis.org/A000217/graph). Hence the lesson
is *not* to generate individual pairs of values; but to *accumulate* values as
they're generated, so later values can be compared against *all* prior ones.

The first step is to generalise the definition of `genUnequal`{.haskell} to take
a `Set`{.haskell} of arbitrarily-many values which our generated `Com`{.haskell}
should be distinct from.  (Note that this may get stuck if the `Set`{.haskell}
already contains all values that can be generated from the chosen amount of
`fuel`{.haskell}; since the number of such values grows exponentially with the
amount of fuel, we're unlikely to hit this problem except for very small cases)

```{.haskell pipe="./show Main.hs"}
-- | Generate a Com, in normal form, which does not appear in the given Set
genDistinctFrom :: Natural -> Set Com -> Gen Com
genDistinctFrom fuel cs = do
  c <- genNormalN fuel
  if Set.member c cs
    then genDistinctFrom fuel cs
    else pure c
```

**Note:** This idea of larger collections leading to many more pairwise
relationships explains the so-called ["birthday paradox"](), and is how
[the known SHA1 collision]() was found.

#### Keep generating until we find what we need ####

We can make use of the above generator in the following, which produces what we
originally wanted: pairs of `Com`{.haskell} values which reduce `v`{.haskell} to
the same thing.  Since we don't know, up front, which of our many generated
values will end up in such pairs, we put them all in a big `Map`{.haskell};
keyed on the value they reduce `v`{.haskell} to (so it only has to calculated
once per value).

```{.haskell pipe="./show Main.hs"}
-- | Generate distinct, normal Com values which have equal normal forms when
-- | applied to the symbolic input 'v'. The result contains two parts: the first
-- | groups together Com values 'f' based on the normal form of 'App f v'; only
-- | groups with more than one element are included. The second Set contains all
-- | of the Com values generated along the way (useful as a pool of distinct Com
-- | values for use in subsequent testing).
-- | The first argument is fuel for generating and normalising Com values. The
-- | second argument is the total number of grouped values to return.
genEqual :: Natural -> Natural -> Gen (Set (Set Com), Set Com)
genEqual fuel = go Map.empty
  where go result 0 = let elems = Set.fromList (Map.elems result)
                       in pure
                            ( Set.filter ((> 1) . Set.size) elems
                            , Set.unions elems )
        go result n = do
          c <- genDistinctFrom fuel (Set.unions (Map.elems result))
          case reduceN (2 * fuel) (App c v) of
            -- If we don't hit a normal form, skip this Com and recurse
            Nothing  -> go result n
            Just key ->
              let single  = Set.singleton c
                  result' = Map.insertWith Set.union key single result
                  matches = Set.toList (Map.findWithDefault single key result')
               in go result' (if length matches > 1 then n - 1 else n)
```

Notice that the unpaired values aren't discarded at the end: instead, they're
all merged into one big `Set`{.haskell}, which we use for the final part.

#### Checking on more than one input ####

The final problem we can solve is to check the behaviour of each pair on *many*
values, rather than just one. Thankfully, we've *already generated* many values!
That's why we return the `Set`{.haskell} of all values we generated in the
course of finding suitable pairs. Our test can loop through all those values,
using them as inputs to each related pair and check whether they match.

### Combining all these improvements ###

Here is the final form of the test I came up with, using the above techniques:

```{.haskell pipe="./show Main.hs"}
-- | Check that Com values that give equal results for a symbolic input, also
-- | give equal results for any other input.
prop_symbolImpliesUniversal :: Property ()
prop_symbolImpliesUniversal = do
    size         <- gen (natRange (5, 10))        -- Max size of each Com value
    count        <- gen (natRange (2, 2 * size))  -- How many values to generate
    (equal, all) <- gen (genEqual size count)
    forM_ equal (assertNoDiff steps all)
  where steps = 100  -- Timeout for normalising on concrete arguments

-- | Check that all elements of the first set give equal results for each of the
-- | elements of the second set. First argument is a timeout when normalising.
assertNoDiff :: Natural -> Set Com -> Set Com -> Property ()
assertNoDiff fuel all equal =
    traverse_ checkPair [(f, g) | f <- eqList, g <- eqList, f /= g]
  where eqList = Set.toList equal
        checkPair (f, g) = traverse_ (assertEqualOn fuel f g) all
```

This test is more reliable than the first, since it does not `discard`{.haskell}
any runs.  It can take longer, e.g. on the order of minutes rather than seconds
(at least for the constants above), since each of the 100 runs is generating and
checking many more values. Thankfully, we know that this effort is being spent
on useful work, rather than duplicate or discarded results.

Augmenting the above with `collect`{.haskell} shows reasonable statistics, for
example:

 - Equality checks do not timeout much (e.g. happening in only around 6% of
   runs). In contrast, *every* run contains some successful equality checks.
 - There is a reasonable spread of group sizes (the number of generated values
   which reduce `v`{.haskell} to the same normal form). Most runs (around 80%)
   found groups of size two (i.e. an individual pair); around 25% found groups
   of size three; ~20% of size four; and so on, up to sizes around 15. Larger
   groups give rise to more pairwise relationships we can check.
 - The total number of generated terms accumulated per run is roughly uniform,
   from around 15 to around 115. Hence each candidate pair is being checked
   against a reasonable amount of example inputs.

## Results ##

## Conclusion ##

I'm pretty happy that I took this little diversion to double-check my assumption
about checking universal quantification through the use of symbolic computation.
The initial test was quick and direct to write, but didn't give me much
confidence that it had explored much of the search space. With a little careful
thought I came up with a much more thorough test, and now I'm quite sure that
this approach is appropriate to use in my egglog code!

Also, this was a nice way to learn the particular API features of falsify, which
is a very nice library that I intend to make more use of!
