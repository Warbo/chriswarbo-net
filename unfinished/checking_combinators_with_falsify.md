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

Recently I was [playing around with the egglog
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

We can "run" an SK program/expression using this `step`{.haskell} function,
which looks for a certain pattern involving a `K`, or a certain pattern
involving an `S`. Remarkably, these two transformations make SK a complete,
universal programming language!


There are a few things to note about this implementation. Unlike in egglog, we
have to explicitly tell Haskell to recursively `step`{.haskell} the children of
an `App`{.haskell} constructor. We also use `Maybe`{.haskell} to indicate
whether any reduction actually took place: this tells us immediately if an
expression has reached a normal form, without having to compare the input to the
output. Finally, notice that *any* `Com`{.haskell} value can be passed around
and rearranged, via the Haskell variables `x`{.haskell}, `y`{.haskell},
`z`{.haskell}, `x'`{.haskell} and `y'`{.haskell}; but only `App`{.haskell}, `C
'K'`{.haskell} and `C 'S'`{.haskell} have any effect on the output (this will be
important for our symbolic execution!).

Next we'll need to *iterate* the `step`{.haskell} function, but that's tricky
since SK is a universal programming language, so we have to account for infinite
loops, long-running computations, exponential memory usage, etc. We do this by
parameterising various functions with "fuel": a `Natural`{.haskell} number
argument which decreases as we progress through a computation; when it hits
zero, we bail out:

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

```{.haskell pipe="./show Main.hs"}
-- | Attempt to reduce a K or S combinator, or the children of an App. Nothing
-- | if the argument is in normal form.
step :: Com -> Maybe Com
step c = stepK c <|> stepS c <|> a l' r' <|> a l' r <|> a l  r'
  where a        = liftA2 App
        (l , r ) = (left c    , right c   )
        (l', r') = (l >>= step, r >>= step)
```

```{.haskell pipe="./show Main.hs"}
-- | Wraps Com values which are assumed to be in normal form
newtype Normal = Normal { toCom :: Com } deriving (Eq, Ord)

instance Show Normal where
  show = show . toCom

-- | Wraps the argument in Normal iff it is in normal form
asNormal :: Com -> Maybe Normal
asNormal c = maybe (Just (Normal c)) (const Nothing) (step c)
```

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

-- | Uses (==) to find identical values
comparer :: Eq a => a -> a -> Compared a
comparer x y = if x == y then Same x else Diff x y

-- | Try normalising the given Com values for the given number of steps. If the
-- | results are == return it in Left; otherwise return both in Right.
normalEqN :: Fuel -> Com -> Com -> Maybe (Compared Normal)
normalEqN n x y = comparer <$> reduceN n x <*> reduceN n y
```

## Property-based testing ##

Let's test this `equalN`{.haskell} function, to see whether it actually behaves
in the way our theory of SK predicts it should. We could start by writing *unit
tests*, but those are limited to [existential quantification](); for example, we
can assert that *some* value is `equalN`{.haskell} to itself:

```{.haskell pipe="./show Main.hs"}
normalEqNToItself :: (Fuel, Com) -> Bool
normalEqNToItself (n, x) = case normalEqN n x x of
    Just (Same _) -> True
    _             -> False
```

```{.unwrap pipe="./run kIsNormalEqNToItself"}
main = assert (normalEqNToItself (0, k)) (putStrLn "PASS")
```

Yet this is is a bit contrived. We really we want to assert that *every* value
is `equalN`{.haskell} to itself (we say `equalN`{.haskell} is
[reflexive](https://en.wikipedia.org/wiki/Reflexive_relation)). Talking about
"every value" is known as [universal
quantification](https://en.wikipedia.org/wiki/Universal_quantification), and
universally-quantified assertions are called "properties". We can represent such
properties as *functions*, which take their quantified variables as arguments:

Properties are a *specification* for our software which is useful for many tasks
including documentation, verification, and testing. To perform such
property-based testing, we give our properties to a "property checker" which
searches for *counterexamples*: argument values which cause the assertion to
fail. If no counterexample can be found, the test passes. Search techniques
range from [simple enumeration](https://hackage.haskell.org/package/smallcheck)
all the way up to [sophisticated AI
algorithms](https://en.wikipedia.org/wiki/American_Fuzzy_Lop_(software)).
Haskell has many property checkers, beginning with the wonderful
[QuickCheck package](https://hackage.haskell.org/package/QuickCheck). We'll use
the state-of-the-art for 2024, which is
[falsify](https://hackage.haskell.org/package/falsify).

### Data generators ###

`falsify` chooses argument values *randomly* from a given "generator", with the
Haskell type `Gen a`{.haskell} representing a generator for values of some type
`a`{.haskell}. These are usually written as combinations of other generators,
ultimately taking fixnums from a particular `Range`{.haskell}. For example, the
following generator for `Natural` is based on `Gen.inRange`{.haskell} (with a
few type conversions, since `Natural`{.haskell} is a bignum not a fixnum):

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

We'll generate `Com`{.haskell} values using the same "fuel" trick as before: to
generate a value with `App`{.haskell} we need to call ourselves recursively
*twice*, so we divide up the fuel (at random) between those calls. Once the fuel
gets too low to divide, we choose between `S` and `K` instead:

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

This "dividing of fuel" approach is my preferred way to generate recursive
datastructures, which I've used in `falsify`, `QuickCheck`, `ScalaCheck`,
`Hypothesis`, etc. because it gives control over the rough "size" of generated
values. In contrast, naïve recursion without a "fuel" parameter generates values
of *exponential* size: either blowing up memory (if recursive calls like
`App`{.haskell} are likely to be chosen) or being limited to a handful of tiny
values (if recursive calls are unlikely to be chosen).

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
-- | Prints "PASS" if 'prop' holds for inputs sampled from 'gen'; otherwise
-- | prints a counterexample and fails.
run :: Show a => (a -> Bool) -> Gen a -> IO ()
run prop gen = shrink prop gen >>= maybe (putStrLn "PASS") abort
  where abort counterexample = fail ("FAIL: " ++ show counterexample)
```

### Included middles ###

The observant among you may have noticed that the property
`prop_equalNIsReflexive`{.haskell} is in fact **false**! `falsify` can generate
a counterexample to show us why:

```{.unwrap pipe="./fail prop_equalNIsReflexive"}
main = run prop_equalNIsReflexive (pair genNat genCom)
```

The precise counterexample `falsify` finds varies depending on the random seed,
but they'll all have the following in common: the fuel will be `0`{.haskell},
whilst the `Com` expression will not be in normal form. For example, it may
produce `KS(SS)` (the Haskell value `App (App k s) (App s s)`{.haskell}).
Since that applies `K` to two values, it can be reduced to the first (`S`) in
one step. The value `KS(SS)` *is* equal to itself (as we expect), but it
requires more than `0`{.haskell} steps to normalise; hence claiming to hold for
*any* number of steps (i.e. universal quantification) was incorrect.

We could alter our claim to *existentially* quantify the number of steps: that
for any SK expression `x`{.haskell}, there is *some* value of `n`{.haskell}
where `equalN n x x` holds. However, that is also false, since there are
infinite loops which have no normal form. There is no way to bound the required
value of `n`{.haskell} (as a function of `x`{.haskell}), since it grows [faster
than any computable function](https://en.wikipedia.org/wiki/Busy_beaver)!

The correct way to fix our claim is to keep both universal quantifiers, but add
two negations to our proposition: instead of claiming every expression is equal
to itself, we claim that *no* expression is *unequal* to itself:

```{.haskell pipe="./show Main.hs"}
notUnnormalEqNToItself :: (Fuel, Com) -> Bool
notUnnormalEqNToItself (n, x) = case normalEqN n x x of
  Just (Diff _ _) -> False
  _               -> True
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

```{.haskell pipe="./show Main.hs"}
normalNormalEqNToItself :: (Fuel, Normal) -> Bool
normalNormalEqNToItself (n, x) = normalEqNToItself (n, toCom x)
```

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

We can now plug in a "reasonable" amount of fuel, to make a simple `genCom`
generator:
```{.unwrap pipe="./run normalNormalEqNToItself"}
main = run normalNormalEqNToItself (pair genFuel genNormal)
```


```{.haskell pipe="./show Main.hs"}
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
