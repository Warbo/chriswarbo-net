---
title: Checking combinators with `falsify`
packages: [ 'haskellWithFalsify' ]
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

## Background ##

I've been playing around with [SK combinatory logic]() recently as a way to
learn [egglog](). I want to implement extensional equivalence, but that requires
*universal quantification*, which egglog isn't suited for. My idea is to
approximate it using symbolic computation, but my initial attempts were not
giving correct results.

This made me doubt my assumption that symbolic variables could be used in place
of universally-quantified expressions; so I decided to check if I was wrong,
using a familiar tool: [property-based testing](). I recently learned about
[the falsify framework for Haskell](), and this seemed like a good way to try it
out!

### Extensional equivalence '''

I was *pretty sure* that the leap from "equal on an unspecified symbol" to
"equal on all possible inputs" is justified, since the symbolic input itself
cannot contribute to the reduction/computation of the result. So, however those
two results end up equal, it does not depend on that input, and hence it holds
regardless of what input we use.

### Symbolic computation ###

### Property-based testing ###

## Implementation ##

Still, I wanted a bit more confidence, so I wrote a little Haskell program using
[the falsify package]() to look for counterexamples.

<details>
<summary>Preamble boilerplate...</summary>

```{pipe="./show Main.hs"}
#!/usr/bin/env runhaskell
module Main (main) where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Natural (Natural, mkNatural)
import qualified Test.Falsify.Generator as Gen
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Predicate as Predicate
import qualified Test.Falsify.Range as Range
import Test.Tasty
import Test.Tasty.Falsify

main :: IO ()
main = defaultMain $ testGroup "Extensionality"
    [ testProperty "Equal for symbol implies equal for any" prop_notUniversal ]
```

</details>

### Implementing combinatory logic ###

We represent combinators in the same way as in egglog. The `deriving` clause
asks Haskell to automatically implement some useful interfaces:

 - `Eq` provides the `==` function. The auto-generated implementation will only
   check whether two `Com` values are identical.
 - `Ord` provides comparisons like `<`. Haskell will generate a lexicographic
   implementation for us, but the details aren't important; having *some*
   implementation of `Ord` lets us use efficient `Set` datastructures.
 - `Show` provides the `show` function, for printing any counterexamples.

```{.haskell pipe="./show Main.hs"}
data Com = C Char | App Com Com deriving (Eq, Ord, Show)
```

Here are our `S` and `K` combinators (Haskell requires the initial letter of a
value name to be lowercase). The `v` combinator will be our "uninterpreted
symbol":

```{.haskell pipe="./show Main.hs"}
s, k, v :: Com
s = C 'S'
k = C 'K'
v = C 'V'  -- Represents an unknown variable
```

Here are the reduction rules for `S` and `K`. Unlike egglog, we have to
explicitly recurse into children of an `App` too. Notice that *any* `Com` value
can be passed around and rearranged, via the Haskell variables `x`, `y`, `z`,
`x'` and `y'`; but only `App`, `C 'K'` and `C 'S'` have any effect on the
output.

We use `Maybe` to indicate whether any reduction actually took place, which lets
us know when an expression has reached a normal form:

```{.haskell pipe="./show Main.hs"}
-- | Attempt to reduce a K or S combinator, or one child of an App. Nothing if
-- | the argument is in normal form.
step :: Com -> Maybe Com
step      (App (App (C 'K') x) _)    = Just x
step (App (App (App (C 'S') x) y) z) = Just (App (App x z) (App y z))
step (App x y) = case (step x, step y) of
  (Just x', _      ) -> Just (App x' y )
  (_      , Just y') -> Just (App x  y')
  _                  -> Nothing
step _ = Nothing
```

Since SK logic is a universal programming language, we have to account for
infinite loops, long-running computations, exponential memory usage, etc. We do
this by parameterising various functions with "fuel": a `Natural` number which
decrements whenever we recurse, and cuts-off the computation when it hits zero:

```{.haskell pipe="./show Main.hs"}
-- | Step the given Com to see if it's in normal form: if so, return it; else
-- | recurse on the reduced form. Gives Nothing if we hit n recursive calls.
reduceN :: Natural -> Com -> Maybe Com
reduceN 0 _ = Nothing
reduceN n c = case step c of
  Just c'  -> reduceN (n-1) c'
  Nothing  -> pure c
```

We can also extend the `==` check, to try normalising the terms for a given
number of steps and comparing the results:

```{.haskell pipe="./show Main.hs"}
-- | Check whether two Com values are equal, after normalising for n steps. When
-- | their normal forms match it's returned as 'Just (Left _)'; unequal normal
-- | forms returned in 'Just (Right (_, _))'; Nothing if they didn't normalise.
equalN n x y = case (reduceN n x, reduceN n y) of
  (Just x', Just y') | x' == y'  -> Just (Left x')         -- Both equal
  (Just x', Just y') | otherwise -> Just (Right (x', y'))  -- Distinct values
  _                              -> Nothing                -- Timeout
```

### Data generators ###

We'll search for counterexamples by generating random data. `falsify` provides a
useful `Range` type for picking random numbers, but it's slightly incompatible
with `Natural` (since the former requires fixnums and the latter is a bignum).
The following generator provides the necessary conversions:

```{.haskell pipe="./show Main.hs"}
-- | Adapts falsify's Range to work for Natural numbers
natRange :: (Natural, Natural) -> Gen Natural
natRange (lo, hi) = mkNatural . pure <$>
  Gen.inRange (Range.between (fromIntegral lo, fromIntegral hi))
```

We'll generate `Com` expressions using a "fuel" parameter, which gets divided
(*unevenly*) between recursive calls for the children of an `App`. Once the
fuel gets too low, we choose an `S` or `K` instead. Note that we won't generate
any other "base" combinators, and in particular we will never generate `v`!

```{.haskell pipe="./show Main.hs"}
-- | Generate a Com, with (roughly) the given number of leaves
genComN :: Natural -> Gen Com
genComN n | n < 2 = Gen.oneof (pure s :| [pure k])
genComN n         = do
  left <- natRange (1, n - 1)
  App <$> genComN left <*> genComN (n - left)
```

This "dividing of fuel" approach is my preferred way to generate recursive
datastructures, which I've used in falsify, QuickCheck, ScalaCheck, Hypothesis,
etc.. This is because it gives control over the rough "size" of the output. In
contrast, naïve recursion without a "fuel" parameter produces outputs of
*exponential* size: either blowing up memory (if `App` is likely to be chosen)
or being limited to a handful of tiny values (if `App` is unlikely to be
chosen).

We won't actually use this generator directly, since it can produce `Com` values
which aren't in normal form (or which *don't have* a normal form). Instead, the
following generator will attempt to normalise the results of `genComN`, and
retry if it finds one that times out:

```{.haskell pipe="./show Main.hs"}
-- | Like genComN, but reduces its outputs to normal form. The fuel
-- | is the size of the generated expression, and (arbitrarily) half
-- | of the attempted normalisation steps.
genNormalN :: Natural -> Gen Com
genNormalN fuel = do
  c <- natRange (0, fuel) >>= genComN
  case reduceN (2 * fuel) c of
    Just c' -> pure c'
    Nothing -> genNormalN fuel
```

We can now plug in a "reasonable" amount of fuel, to make a simple `genCom`
generator:

```{.haskell pipe="./show Main.hs"}
-- | Reasonably sized Com values
genCom = natRange (0, 20) >>= genComN
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

 - It's quite unlikely to generate suitable values for `f` and `g`, which
   just-so-happen to reduce `v` to the same value. Hence all of those discarded
   runs. The test harness will give up if too many runs are discarded, which
   makes the test flaky.
 - When we *do* generate a pair of suitable values, they are often ones that
   were already tested in a prior run. For example, using `collect` to gather
   statistics about suitable values shows them to be `K` 16% of the time and
   `S` 11% of the time (this varies between test invocations). This wastes
   resources and makes the "successful tests" number misleadingly inflated.
 - When we *do* happen to generate a successful pair of values, we're only
   checking them on a *single* argument, which isn't very thorough.

### A more sophisticated test ###

We're going to come up with a much better test, by applying a few simple ideas.
The result will be a bit more complicated, but more reliable and (crucially)
subject our assumption to a *much* more thorough barrage of checks.

#### Smarter generators to avoid discarding ####

The pattern-match in `prop_simplisticTest` will `discard` the test unless very
specific requirements are met. One is that `f /= g` (i.e. `f` and `g` should not
be equal). Discarding an entire test run is a pretty wasteful way of ensuring
this precondition: especially since we *already have* the value of `f` when we
generate `g`! Here's a smarter generator, which keeps retrying until it makes a
value that's different from the given argument:

```{pipe="./show Main.hs"}
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

The real problem is that a pair of values `f` and `g` are very unlikely to be
related in the way we need (that they reduce the input `v` to the same value).
However, the structure of our test is quite wasteful of the opportunities it
has: when a test run is discarded, we *also* discard the generated values of `f`
and `g` values; let's call them `f1` and `g1`. On the next iteration we generate
a fresh pair, say `f2` and `g2`; but we only compare those new values to each
other, and ignore the discarded values.

What if we *also* compared `f2` with `f1`, and `f2` with `g1`, and `g2` with
`f1` and `g2` with `g1`? There are a total of *six* possible relationships
between the values we've generated; but we're currently only checking ⅓ of them.
If we also discard that second pair and generate a fresh `f3` and `g3`, that's
*fifteen* possible relationships between the values we've generated, of which
we're only checking ⅕. Indeed, as we generate more and more values the number of
possible pairings follows
[sequence A000217 of the OEIS](https://oeis.org/A000217/graph). Hence the lesson
is *not* to generate individual pairs of values; but to *accumulate* values as
they're generated, so later values can be compared against *all* prior ones.

The first step is to generalise the definition of `genUnequal` to take a `Set`
of arbitrarily-many values which our generated `Com` should be distinct from.
(Note that this may get stuck if the `Set` already contains all values that can
be generated from the chosen amount of `fuel`; since the number of such values
grows exponentially with the amount of fuel, we're unlikely to hit this problem
except for very small cases)

```{pipe="./show Main.hs"}
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
originally wanted: pairs of `Com` values which reduce `v` to the same thing.
Since we don't know, up front, which of our many generated values will end up in
such pairs, we put them all in a big `Map`; keyed on the value they reduce `v`
to (so it only has to calculated once per value).

```{pipe="./show Main.hs"}
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
all merged into one big `Set`, which we use for the final part.

#### Checking on more than one input ####

The final problem we can solve is to check the behaviour of each pair on *many*
values, rather than just one. Thankfully, we've *already generated* many values!
That's why we return the `Set` of all values we generated in the course of
finding suitable pairs. Our test can loop through all those values, using them
as inputs to each related pair and check whether they match.

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

This test is more reliable than the first, since it does not `discard` any runs.
It can take longer, e.g. on the order of minutes rather than seconds (at least
for the constants above), since each of the 100 runs is generating and checking
many more values. Thankfully, we know that this effort is being spent on useful
work, rather than duplicate or discarded results.

Augmenting the above with `collect` shows reasonable statistics, for example:

 - Equality checks do not timeout much (e.g. happening in only around 6% of
   runs). In contrast, *every* run contains some successful equality checks.
 - There is a reasonable spread of group sizes (the number of generated values
   which reduce `v` to the same normal form). Most runs (around 80%) found
   groups of size two (i.e. an individual pair); around 25% found groups of size
   three; ~20% of size four; and so on, up to sizes around 15. Larger groups
   give rise to more pairwise relationships we can check.
 - The total number of generated terms accumulated per run is roughly uniform,
   from around 15 to around 115. Hence each candidate pair is being checked
   against a reasonable amount of example inputs.

## Results ##

```{pipe="sh"}
./Main.hs
```

## Conclusion ##

I'm pretty happy that I took this little diversion to double-check my assumption
about checking universal quantification through the use of symbolic computation.
The initial test was quick and direct to write, but didn't give me much
confidence that it had explored much of the search space. With a little careful
thought I came up with a much more thorough test, and now I'm quite sure that
this approach is appropriate to use in my egglog code!

Also, this was a nice way to learn the particular API features of falsify, which
is a very nice library that I intend to make more use of!