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
{
  echo '```haskell'
  tee -a "$1"
  printf '\n```\n\n```{pipe="sh"}\ncp root/%s ./\nrunhaskell %s%s\n```\n' \
    "$1" "$1" "$2"
} | pandoc -f markdown -t json | panpipe
```

```{pipe="cat > fail && chmod +x fail"}
#!/bin/sh
# Retry a few times, in case falsify random sampling fails to find a
# counterexample we wanted to show!
GIVEN=$(cat)
for RETRY in seq 1 100
do
  if GOT=$(echo "$GIVEN" | ./run "$@" ' 2>&1 && exit 1 || true')
  then
    echo "$GOT"
    exit 0
  fi
  done
exit 1
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
import Data.Char (chr, ord)
import Data.Foldable
import Data.List (inits)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Natural (Natural, mkNatural)
import Test.Falsify.Generator (Gen, Tree(..), (:->))
import qualified Test.Falsify.Generator as Gen
import Test.Falsify.Interactive (shrink)
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as Predicate
import Test.Falsify.Range (Range)
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

-- | Tries to step the given Com: if it worked, returns that in Left; otherwise
-- | returns the original value (now Normal) in Right.
toNormal :: Com -> Either Com Normal
toNormal c = case step c of
  Nothing -> Right (Normal c)
  Just c' -> Left c'
```

`toNormal`{.haskell} tells us when to stop iterating, but since SK is a
universal programming language, there's no way to know beforehand if it ever
will. We'll account for this by wrapping such undecidable computations in
[a `Delay`
type](http://www.chriswarbo.net/blog/2014-12-04-Nat_like_types.html#delay-t):

```{.haskell pipe="./show Main.hs"}
data Delay a = Now a | Later (Delay a) deriving (Eq, Functor, Ord, Show)

instance Applicative Delay where
  (Now   f) <*> x =        f <$> x
  (Later f) <*> x = Later (f <*> x)
  pure            = Now

instance Monad Delay where
  (Now   x) >>= f = f x
  (Later x) >>= f = Later (x >>= f)
```

Since `Delay`{.haskell} could be a never-ending chain of `Later`{.haskell}
wrappers, any attempt to extract a value from one must eventually give up. We
can represent this using parameters of type `Fuel`{.haskell} (a synonym for
natural numbers):

```{.haskell pipe="./show Main.hs"}
-- | Represents a parameter for "when to give up"
type Fuel = Word

-- | Try to extract a value from the given 'Delay'
delayed :: Fuel -> Delay a -> Maybe a
delayed n x = case x of
  Now   x' -> Just x'
  Later x' -> guard (n > 0) *> delayed (n - 1) x'

-- | Try to extract a value from the given 'Delay', or use the given default
delayedOr :: Fuel -> a -> Delay a -> a
delayedOr n def x = maybe def id (delayed n x)
```

Now we can iterate the `toNormal`{.haskell} function, hiding each recursive call
safely beneath a `Later`{.haskell} wrapper:

```{.haskell pipe="./show Main.hs"}
-- | Step the given Com until it reaches normal form.
reduce :: Com -> Delay Normal
reduce c = case toNormal c of
  Right n -> Now n
  Left c' -> Later (reduce c')
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
normalEq :: Com -> Com -> Delay (Compared Normal)
normalEq x y = comparer <$> reduce x <*> reduce y
```

## Property-based testing ##

Let's test this `normalEq`{.haskell} function, to see whether it actually behaves
in the way our theory of SK predicts it should. For example, every equality
relation should have the
["reflexive property"](https://en.wikipedia.org/wiki/Reflexive_relation),
meaning that values should be equal to themselves. The following predicate
(function returning a boolean) checks whether its argument is `normalEq`{.haskell}
to itself:

```{.haskell pipe="./show Main.hs"}
normalEqToItself :: (Fuel, Com) -> Bool
normalEqToItself (n, x) = delayedOr n False (same <$> normalEq x x)
```

We can turn this predicate into a general statement by *quantifying* its
argument. It's common to use [existential
quantification](https://en.wikipedia.org/wiki/Existential_quantification), which
asserts that *some* argument satisfies `normalEqToItself`{.haskell}. This is the
widely practiced "example-based" approach to automated testing. For example:

```{.unwrap pipe="./run kIsNormalEqToItself"}
main = assert (normalEqToItself (0, k)) (putStrLn "PASS")
```

However, that isn't really what we want to say: reflexivity doesn't just apply
to a few hand-picked examples, it means that *every argument* satisfies
`normalEqToItself`{.haskell}. Talking about "every argument" is [universal
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
-- | A reasonable default for procedures requiring Fuel. Small enough to keep
-- | exponentially-growing terms from blowing up.
limit :: Fuel
limit = 20

-- | We usually want Ranges from zero upwards
to :: Fuel -> Range Fuel
to = Range.between . (0,)

-- | A reasonable Range of Fuel to use in tests
small :: Range Fuel
small = to limit

-- | Generates up to a certain amount of Fuel
genFuelN :: Fuel -> Gen Fuel
genFuelN = Gen.inRange . to

-- | Generates a (relatively small) amount of Fuel
genFuel = Gen.inRange small
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

The observant among you may have noticed that `normalEqToItself`{.haskell}
**does not** hold for all argument values! `falsify` can generate a
counterexample to show us why:

```{.unwrap pipe="./fail normalEqToItself"}
main = run normalEqToItself (pair genFuel genCom)
```

The precise counterexample `falsify` finds may vary depending on the random
seed, but they'll all have the following in common: the `Fuel`{.haskell} will be
`0`{.haskell}, whilst the `Com` expression will not be in normal form. For
example, it may produce `KKK` (the Haskell value `App (App k k) k`{.haskell}).
`stepK`{.haskell} will reduce that to a single `K`, so whilst the value
`KKK` *is* equal to itself (as we expect), `normalEq`{.haskell} wraps it in a
`Later`{.haskell} constructor, which may cause `normalEqToItself`{.haskell} to
give up *some* `Fuel`{.haskell} parameters: in particular, when given
`0`{.haskell} `Fuel`{.haskell}. This disproves our claim that the property holds
for *any* amount of `Fuel`{.haskell}.

We could alter our claim to *existentially* quantify the amount of
`Fuel`{.haskell}: that for any SK expression `x`{.haskell}, there is *some*
value of `n`{.haskell} where `delayed n (normalEq x x)`{.haskell} holds. However
that is *also* false, since there are infinite loops which have no
`Normal`{.haskell} form; hence never produce a `Now` value; and therefore cannot
have a result extracted regardless of how much `Fuel`{.haskell} we use. Even
when a `Normal`{.haskell} form *does* exist, there is no way to bound
[the amount of `Fuel`{.haskell} required to find
it](https://en.wikipedia.org/wiki/Busy_beaver).

The correct way to fix our claim is to universally quantify `x`{.haskell} and
`n`{.haskell} as before, but add two negations to our predicate: instead of
claiming every expression is equal to itself (regardless of `Fuel`{.haskell}),
we claim that every expression is *not unequal* to itself (regardless of
`Fuel`{.haskell}):

```{.haskell pipe="./show Main.hs"}
notUnnormalEqToItself :: (Fuel, Com) -> Bool
notUnnormalEqToItself (n, x) = delayedOr n True (not . diff <$> normalEq x x)
```

```{.unwrap pipe="./run notUnnormalEqToItself"}
main = run notUnnormalEqToItself (pair genFuel genCom)
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
`notUnnormalEqToItself`{.haskell}, which make strong claims over a broad
space of input values. Sometimes it's a good idea to *also* have more specific
properties tailored to important cases. For example, the predicate
`normalEqToItself`{.haskell} is actually `True`{.haskell} for the
`Normal`{.haskell} subset of `Com`{.haskell}:

```{.haskell pipe="./show Main.hs"}
normalsAreNormalEqToThemselves :: (Fuel, Normal) -> Bool
normalsAreNormalEqToThemselves (n, x) = normalEqToItself (n, toCom x)
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
genNormalN n = do
  c <- genComN n                -- Generate a Com value c
  maybe (genNormalN n)          -- Retry if c didn't finish reducing
        return                  -- Return the Normal value if reduction finished
        (delayed n (reduce c))  -- Try to reduce c to a Normal value

-- | Generates (relatively small) Normal values
genNormal :: Gen Normal
genNormal = genFuel >>= genNormalN
```

```{.unwrap pipe="./run normalsAreNormalEqToThemselves"}
main = run normalsAreNormalEqToThemselves (pair genFuel genNormal)
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
agree :: Com -> Com -> InputValues -> Delay (Compared Normal)
agree f g (iv:ivs) = agree (App f iv) (App g iv) ivs  -- Apply an input, recurse
agree f g []       = normalEq f g                     -- No more inputs, check
```

Everything that satisfies `normalEq`{.haskell} should also satisfy
`agree`{.haskell}, which we can state with the following property:

```{.haskell pipe="./show Main.hs"}
normalEqImpliesAgree :: (Fuel, Com, Com, InputValues) -> Bool
normalEqImpliesAgree (n, f, g, xs) =
  case delayed n (pair (normalEq f g) (agree f g xs)) of
    Just (Same _, Diff _ _) -> False
    _                       -> True

-- | Generate a list of Com values, with length and element size bounded by Fuel
genComsN :: Fuel -> Gen [Com]
genComsN fuel = do max <- genFuelN fuel
                   Gen.list (Range.between (0, max)) (genComN fuel)

-- | Generate (relatively small) lists of Com values
genComs :: Gen InputValues
genComs = genFuel >>= genComsN
```

```{.unwrap pipe="./run normalEqImpliesAgree"}
main = run normalEqImpliesAgree (quad genFuel genCom genCom genComs)
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
    delayedOr n True (not . diff <$> agree f g [x, y])
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
-- | False iff the Com values agree on xs but not xs++ys
agreementIsMonotonic :: (Fuel, (Com, Com), (InputValues, InputValues)) -> Bool
agreementIsMonotonic (n, (f, g), (xs, ys)) =
  case delayed n (pair (agree f g xs) (agree f g (xs ++ ys))) of
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

Once we've normalised a `Com`{.haskell} value containing symbols, we need a
semantics, or [abstract
interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation), to
understand what they mean. Since we're using each symbol to represent a
distinct input, the way they appear in `Normal`{.haskell} forms tells us
something about the overall behaviour of the expression and, crucially, how it
depends on the particular *concrete* `Inputs`{.haskell} it may be applied to.
It's undecidable whether part of an SK expression will influence its reduction
(or just get discarded by the `stepK`{.haskell} rule), so we'll limit our
ambitions to understanding a few simple situations.

### Symbolic agreement proves extensional equality ###

Expressions which agree on symbolic inputs will agree on *all* inputs, since
symbols do not reduce, and hence those inputs must have been irrelevant for the
reductions which lead to the agreement. Unlike property-checking, where we rely
on double-negatives to "fail to disprove agreement", this is real positive proof
that expressions will *always* agree, and hence we can claim definitively that
they *are* extensionally equal.

We don't need to implement this check specially, since we can reuse
`agree`{.haskell}, just using symbolic values as our inputs instead of concrete
SK expressions. The following `agreeSym` function applies two expressions to
more and more symbolic inputs, to see if they reduce to the same
`Normal`{.haskell} form:

```{.haskell pipe="./show Main.hs"}
-- | All values 'C x', except s and k, for use as uninterpreted symbols
symbols :: InputValues
symbols = filter (/= k) . filter (/= s) . map C $ [minBound..maxBound]

-- | Checks whether two Com values agree on more and more symbolic input values.
agreeSym :: Com -> Com -> [Delay (Compared Normal)]
agreeSym f g = agree f g <$> inits symbols
```

Here are a couple of sanity checks. Firstly, it should always spot expressions
which are normally equivalent (since they agree on zero inputs):

```{.haskell pipe="./show Main.hs"}
-- | Try to extract values from the first few list elements.
delayeds :: Fuel -> [Delay a] -> [a]
delayeds n = catMaybes . take (fromIntegral n + 1) . map (delayed n)

normalEqImpliesAgreeSym :: (Fuel, Com, Com) -> Bool
normalEqImpliesAgreeSym (n, x, y) =
    if delayedOr n False (same <$> normalEq x y)
       then any same (delayeds n (agreeSym x y))
       else True
```

```{.unwrap pipe="./run normalEqImpliesAgreeSym"}
main = run normalEqImpliesAgreeSym (triple genFuel genCom genCom)
```

This check should also be monotonic, i.e. checking with more `Fuel`{.haskell}
should never *prevent* an agreement being found:

```{.haskell pipe="./show Main.hs"}
agreeSymIsMonotonic :: (Fuel, Fuel, Com, Com) -> Bool
agreeSymIsMonotonic (n, m, x, y) =
    if any same (results n)
       then any same (results (n + m))
       else True
  where results = (`delayeds` agreeSym x y)
```

```{.unwrap pipe="./run agreeSymIsMonotonic"}
main = run agreeSymIsMonotonic (quad genFuel genFuel genCom genCom)
```

### Different symbolic heads prove disagreement ###

The head of an expression determines how it will reduce. When a symbolic input
appears as the head, the expression's `Normal`{.haskell} form depends entirely
on that input. Expressions with *different* symbolic inputs as their heads
depend on separate inputs, and can hence be forced to disagree. For example, say
`Xab` is some concrete SK expression applied to two symbolic inputs, which
happens to reduce to `aS`; hence its result depends on the *first* input. If
some other expression `Yab` reduces those inputs to `bS`, its result depends on
the *second* input. We can force them to disagree on a pair of inputs, by using
the first to control the result of `X` and the second to control the result of
`Y`. In this case we could use input values `KS` and `KK`, which `X` will reduce
to `S` and `Y` reduces to `K`, hence disagreeing.

The following function will spot when two expressions have different symbols in
head position:

```{.haskell pipe="./show Main.hs"}
-- | Return the head (left-most leaf) of the given Com
headPos :: Com -> Com
headPos (C c)     = C c
headPos (App l r) = headPos l

-- | Whether the given Com is an uninterpreted symbol used by symbolic execution
isSym :: Com -> Bool
isSym c@(C _) = c /= s && c /= k
isSym _       = False

-- | Whether the given Com values have distinct symbolic values in head position
distinctSymbolicHeads :: Com -> Com -> Bool
distinctSymbolicHeads x y = isSym hX && isSym hY && hX /= hY
  where (hX, hY) = (headPos x, headPos y)

-- | Generate any Char value
genChar :: Gen Char
genChar = chr <$> Gen.inRange charRange
  where charRange  = Range.between (ord minBound, ord maxBound)

-- | Generate Com values which may also contain symbols
genSymComN :: Fuel -> Gen Com
genSymComN n = toSymCom <$> Gen.tree (Range.between (0, n)) genChar
  where toSymCom t = case t of
          Leaf                               -> k
          Branch _ Leaf Leaf                 -> k
          Branch _ Leaf (Branch _ Leaf Leaf) -> s
          Branch c (Branch _ Leaf Leaf) Leaf -> C c
          Branch _ l r                       -> App (toSymCom l) (toSymCom r)

-- | Generate (relatively small) Com values which may contain symbols
genSymCom = genFuel >>= genSymComN

-- | Shouldn't matter which order we compare two Com values
distinctSymbolicHeadsCommutes :: (Com, Com) -> Bool
distinctSymbolicHeadsCommutes (x, y) = distinctSymbolicHeads x y
                                    == distinctSymbolicHeads y x
```

```{.unwrap pipe="./run distinctSymbolicHeadsCommutes"}
main = run distinctSymbolicHeadsCommutes (pair genSymCom genSymCom)
```

### Different numbers of arguments prove disagreement ###

Expressions whose heads are *the same* symbolic input, but applied to a
different number of arguments, can also be forced to disagree. For example, if
`Xab` reduces to `aSb` and `Yab` reduces to `aS`, then
`distinctSymbolicHeads`{.haskell} can't be sure if they're different. Yet we can
still force them to disagree, by giving them *three* inputs (`a`, `b` and `c`):
we choose a value for `a` which consumes two arguments and reduces to the
second, such as `SK`; we give `b` the value `Kd` (where `d` is a symbol); and we
leave `c` symbolic. `X` and `Y` will reduce these inputs as follows:

```
X(SK)(Kd)       c    |    Y(SK)(Kd)c        | Xabc and Yabc
  SKS(Kd)       c    |      SKS    c        | Reduced per example
   K (Kd)(S(Kd))c    |       K     c(Sc)    | Applied S rule to each
      Kd        c    |             c        | Applied K rule to each
       d             |             c        | Applied K rule again to first
```

Hence `X` and `Y` can be reduced to different symbols, so we can force them to
disagree by replacing the symbols `c` and `d` with unequal values, say `S` and
`K`.

The following `unequalArgCount`{.haskell} function will spot when two
expressions apply a symbol to an unequal number of arguments:

```{.haskell pipe="./show Main.hs"}
-- | Split a Com value into its head and any arguments that's applied to
headAndArgs :: Com -> (Com, [Com])
headAndArgs x@(C _) = (x, [])
headAndArgs (App l r) = case headAndArgs l of
  (h, args) -> (h, args ++ [r])

-- | True iff the given expressions have the same symbol in their head, but
-- | applied to a different number of arguments.
unequalArgCount :: Com -> Com -> Bool
unequalArgCount x y = isSym headX
                   && headX == headY
                   && length argsX /= length argsY
  where (headX, argsX) = headAndArgs x
        (headY, argsY) = headAndArgs y

-- | Order of Com values shouldn't affect result
unequalArgCountCommutes :: (Com, Com) -> Bool
unequalArgCountCommutes (x, y) = unequalArgCount x y
                              == unequalArgCount y x
```

```{.unwrap pipe="./run unequalArgCountCommutes"}
main = run distinctSymbolicHeadsCommutes (pair genSymCom genSymCom)
```

### Disagreeing arguments prove disagreement ###

Finally, if symbolic heads are given the same *number* of arguments, we will see
if their *values* disagree; in which case, we can choose input values which
propagate such disagreements to the top-level. For example, let's say `Xabc`
reduces to `ab` and `Yabc` reduces to `ac`: these are unequal, but our
`distinctSymbolicHeads`{.haskell} function can't tell (since their heads have
the same symbol, `a`); and our `unequalArgCount`{.haskell} function can't tell
(since they apply `a` to the same number of arguments, one). However, we *can*
tell that those argument *values* are unequal (since one is `b` and the other is
`c`, which `distinctSymbolicHeads`{.haskell} can tell are unequal).

In this case we can force a disagreement between `X` and `Y` by choosing a value
for the first input which reduces to its first argument, say `SKK`. Then
`X(SKK)bc` reduces to `b` and `Y(SKK)bc` reduces to `c`, which we can force to
disagree using unequal input values like `S` and `K`.

The following function checks for the situation described above, using a given
function to check whether two arguments are unequal:

```{.haskell pipe="./show Main.hs"}
-- | Whether the given Com values have matching symbols in their heads, but
-- | applied to unequal values (determined by the given unEq function)
symbolGivenUnequalArgs :: (Com -> Com -> Bool) -> Com -> Com -> Bool
symbolGivenUnequalArgs unEq x y = isSym headX
                               && headX == headY
                               && any id (zipWith unEq' argsX argsY)
  where (headX, argsX) = headAndArgs x
        (headY, argsY) = headAndArgs y
        unEq' a b      = unEq a b || unEq b a  -- Compare both ways round

-- | The order of arguments shouldn't alter the result
symbolGivenUnequalArgsCommutes :: (Com -> Com -> Bool) -> Com -> Com -> Bool
symbolGivenUnequalArgsCommutes f x y = symbolGivenUnequalArgs f x y
                                    == symbolGivenUnequalArgs f y x
```

<details class="odd">
<summary>How to generate a function which accepts `Com`{.haskell}
arguments...</summary>

The `symbolGivenUnequalArgs`{.haskell} function doesn't care *how* arguments are
deemed to be unequal, so it lets the caller decide by passing in a function. The
choice of function shouldn't affect commutativity, so the property
`symbolGivenUnequalArgsCommutes`{.haskell} is universally quantified over that
choice; allowing a counterexample search to try many different functions.

This may seem surprising if you've not encountered it before, but we can
generate a function value just as easily as a "data-like" value. For example, we
can generate functions which look up their arguments in a (generated) lookup
table:

```{.haskell pipe="./show Main.hs"}
genViaTable :: Eq a => Range Fuel -> Gen a -> Gen b -> Gen (a -> b)
genViaTable range genA genB = do
  def   <- genB
  pairs <- Gen.list range (pair genA genB)
  pure (maybe def id . (`lookup` pairs))
```

However, such ordinary function values are opaque "black boxes", which makes
them less useful for property-checking: firstly, it's hard to `show`{.haskell}
them (e.g. if they form part of a counterexample); and secondly it's hard to
shrink them (to look for "simpler" alternatives). Property-checkers avoid these
problems by replacing ordinary functions `a -> b`{.haskell} with their own
alternative, whose constructors 'remember' how to `show`{.haskell} and shrink
themselves.  In `falsify` this alternative is `Gen.Fun a b`{.haskell}, so we'll
need to convert the `Com -> Com -> Bool` argument of
`symbolGivenUnequalArgsCommutes`{.haskell} into either
`Fun Com (Fun Com Bool)`{.haskell}
([curried](https://en.wikipedia.org/wiki/Currying) form) or, equivalently,
`Fun (Com, Com) Bool`{.haskell} (uncurried form). We'll use the uncurried form,
since the tuples will be handled automatically by type class instance
resolution. The following helpers let `symbolGivenUnequalArgsCommutes`{.haskell}
take a `Fun` as argument:

```{.haskell pipe="./show Main.hs"}
-- | Transform a higher-order function to take a Fun instead
liftFun :: ((a -> b) -> c) -> Gen.Fun a b -> c
liftFun f = f . Gen.applyFun

-- | Lift a function with binary argument to take an uncurried Fun instead
liftFun2 :: ((a -> b -> c) -> d) -> Gen.Fun (a, b) c -> d
liftFun2 f = liftFun (f . curry)

-- | Like 'uncurry', but for three arguments
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
```

To check (the lifted version of) `symbolGivenUnequalArgsCommutes`{.haskell}, we
need a generator of type `Gen (Fun (Com, Com) Bool`{.haskell}. We can get one
from `Gen.fun`{.haskell}, but that requires an instance of
`Gen.Function (Com, Com)`{.haskell}. There's an existing instance for tuples,
but we still need to implement `Gen.Function Com`{.haskell} ourselves:

```{.haskell pipe="./show Main.hs"}
instance Gen.Function Com where
  function gen = Gen.functionMap comToPre preToCom <$> Gen.function gen
```

This is piggybacking on an existing instance (the `Gen.function gen`{.haskell}
call at the end) by converting our `Com`{.haskell} values back-and-forth to
another type that already implements `Gen.Function`{.haskell}. The type I've
chosen is `[Maybe (Either Bool Char)]`{.haskell}, and the conversions are
implemented by `comToPre`{.haskell} & `preToCom`{.haskell}:

```{.haskell pipe="./show Main.hs"}
-- | Prefix notation for expressions. `Nothing` represents an 'apply' operation.
type Prefix = [Maybe (Either Bool Char)]

-- | Converts expressions from "applicative style" Com to "prefix style" Prefix.
comToPre :: Com -> Prefix
comToPre x = case x of
  -- Treat K and S specially, so they're more likely to be generated
  _ | x == k -> [Just (Left False)]
  _ | x == s -> [Just (Left True )]
  -- Any other symbol
  C c        -> [Just (Right c)]
  -- Nothing acts like an apply operator for two expressions that follow it
  App l r    -> [Nothing] ++ comToPre l ++ comToPre r

-- | Parses "prefix style" Prefix back into an "applicative style" Com.
preToCom :: Prefix -> Com
preToCom = fst . go          -- Discard any remaining suffix
  where go []     = (k, [])  -- Makes preToCom total, but overlaps 'Left False'
        go (x:xs) = case x of
          Just (Left False) -> (k  , xs)
          Just (Left True ) -> (s  , xs)
          Just (Right c   ) -> (C c, xs)
          Nothing           -> let (l, ys) = go xs
                                   (r, zs) = go ys
                                in (App l r, zs)

comToPreRoundtrips :: Com -> Bool
comToPreRoundtrips c = preToCom (comToPre c) == c
```

```{.haskell pipe="./run comToPreRoundtrips"}
main = run comToPreRoundtrips genCom
```

```{.haskell pipe="./show Main.hs"}
preToComRoundtrips :: Prefix -> Bool
preToComRoundtrips p = comToPre (preToCom p') == p'
  where p' = comToPre (preToCom p)  -- Extra roundtrip to avoid extra suffix

genPre :: Gen Prefix
genPre = Gen.list small genEntry
  where genEntry = Gen.choose (Just <$> genLeaf) (pure Nothing)
        genLeaf  = Gen.choose (Left <$> Gen.bool False) (Right <$> genChar)
```

```{.haskell pipe="./run preToComRoundtrips"}
main = run preToComRoundtrips genPre
```

The `Prefix`{.haskell} is a
[prefix form](https://en.wikipedia.org/wiki/Polish_notation) for expressions, as
opposed to the "applicative form" of `Com`{.haskell}. They're equivalent, but
the structure of an expression is less obvious in prefix form, which is why it
only appears in this hidden section. This is the encoding used in [binary
combinatory logic](https://en.wikipedia.org/wiki/Binary_combinatory_logic),
although we're allowing arbitrary `Char`{.haskell} values rather than just `S`
and `K`.

The functions `preToCom`{.haskell} and `comToPre`{.haskell} don't *quite* form
an [isomorphism](https://en.wikipedia.org/wiki/Isomorphism). This is essentially
due to `Prefix`{.haskell} being a [prefix(-free)
code](https://en.wikipedia.org/wiki/Prefix_code):

 - The empty list `[]`{.haskell} does not correspond to a particular
   `Com`{.haskell} value. It a valid *prefix*, since appending it with anything
   will either form a complete expression or another valid prefix; but that's
   not so useful for `preToCom`{.haskell}, which might have only been given a
   *finite* list and needs to return a `Com`{.haskell}. In this case, it just
   returns `K` (arbitrarily).
 - `Prefix`{.haskell} values may contain "too many" `Nothing`{.haskell} values.
   These act like `App`{.haskell}, indicating that the following expression is
   applied to the one after. That "following expression" may itself use
   `Nothing`{.haskell} to consume subsequent list elements, and so on;
   corresponding to the arbitrary nesting which `App`{.haskell} allows. However,
   the given `Prefix`{.haskell} may "run out" before specifying what those
   following elements should be! Again, that's a perfectly good *prefix*, but
   not a complete value. When this happens, we will hit the same `[]`{.haskell}
   case as above, and simply return `K` (potentially many times, as the
   applications get "popped off the stack").
 - Expressions encoded as `Prefix`{.haskell} values are self-delimiting, meaning
   that the encoding itself tells us when to finish parsing: if the value we
   parsed was `Just _`{.haskell}, we have finished that expression; if the value
   is `Nothing`{.haskell} we can finish after parsing exactly two more
   expressions. Hence any elements appearing *after* a correctly-encoded
   expression will be completely ignored, resulting in the same `Com`{.haskell}.
 - Our hacky use of `K` as a fallback for empty `Prefix`{.haskell} values
   introduces some ambiguity, since an expression like `KK` could be encoded as
   `[Nothing, Just (Left False), Just (Left False)]`{.haskell} (the "correct"
   way); or as `[Nothing, Just (Left False)]`{.haskell} (relying on the fallback
   behaviour to introduce another `K`); or indeed as `[Nothing]`{.haskell}
   (relying on the fallback for *both* `K` expressions). Hence the fallback
   behaviour should not be relied upon; also, it breaks when composing
   expressions together (since missing `K` values will only be introduced at the
   end of a list).
 - There's also some redundancy between
   `Left False`{.haskell}/`Left True`{.haskell} and
   `Right 'K'`{.haskell}/`Right 'S'`{.haskell}. This was a deliberate choice, to
   make `falsify` generate more `S` and `K` expressions.

These aren't a problem for our use of `Prefix`{.haskell} in generating values;
but you may need to keep them in mind when using this encoding for other
purposes (in which case I'd recommend *embracing* its nature as a prefix(-free)
code, since it has all sorts of cool applications!)

</details>

```{.unwrap pipe="./run symbolGivenUnequalArgsCommutes"}
main = run (uncurry3 (liftFun2 symbolGivenUnequalArgsCommutes))
           (triple (Gen.fun (Gen.bool False)) genSymCom genSymCom)
```

### Combining disagreement provers ###

We can never spot *all* disagreements, but the simple checks above can be
combined into a single, reasonably-useful function. Not only is this easier to
use than the individual checks, but we can also use it as the `unEq`{.haskell}
argument of the `symbolGivenUnequalArgs`{.function}, which lets us recurse
through the given expressions looking for disagreement at any depth!

```{.haskell pipe="./show Main.hs"}
provablyDisagree :: Com -> Com -> Bool
provablyDisagree x y = distinctSymbolicHeads                   x y
                    || unequalArgCount                         x y
                    || symbolGivenUnequalArgs provablyDisagree x y

provablyDisagreeCommutes :: (Com, Com) -> Bool
provablyDisagreeCommutes (x, y) = provablyDisagree x y == provablyDisagree y x
```

```{.unwrap pipe="./run provablyDisagreeCommutes"}
main = run provablyDisagreeCommutes (pair genSymCom genSymCom)
```

```

```{.unwrap pipe="./run extensionallyEqImpliesAgree"}
main = run extensionallyEqImpliesAgree (triple genCom genCom genComs)
```

### A simplistic first attempt ###

We can now *directly* test our assumption that equal results on a symbolic input
imply equal results on all inputs, like like this:

```{.haskell pipe="./show Main.hs"}
-- | Asserts that the first two Com values give equal results when applied to
-- | the third. First argument limits the number of steps attempted.
assertEqualOn n f g x = case delayed n (normalEq fx gx) of
  Just (Right (fx', gx')) -> testFailed
    (concat [show fx, " -> ", show fx', "\n", show gx, " -> ", show gx'])
  _ -> pure ()
  where fx = App f x
        gx = App g x

-- | Generate two distinct Com values which normalise the same way when applied
-- | to a symbolic variable: check that they agree for a third Com value too.
prop_simplisticTest = do
    -- Generate a couple of Com values
    f <- genCom
    g <- genCom
    case delayed steps (normalEq (App f v) (App g v)) of
      -- When f and g are distinct and agree for v, try them on another input
      Just (Left _) | f /= g -> do
        x <- genCom
        assertEqualOn steps f g x
      -- When f and g disagree, discard them and try again
      _ -> discard
  where steps = 100
        genCom = gen (natRange (0, 20) >>= genComN)
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
genUnequal :: Natural -> Com -> Gen Com
genUnequal n x = do
  c <- genComN n
  if c == x
    then genUnequal n x
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
          case delayed (2 * fuel) (reduce (App c v)) of
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
