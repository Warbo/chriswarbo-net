---
title: "SK logic in egglog: part 3, falsifying myself"
packages: [ 'ghcWithFalsify' ]
---

In the previous part we learned some background theory about SK logic, and built
up our notion of equality from simple identity (`==`{.haskell}), through
equivalence of `Normal`{.haskell} forms (`normalEq`{.haskell}) up to full
extensional equality (`extEq`{.haskell}). In this post we'll investigate these
definition more thoroughly. Whilst the eventual aim is to find the mistake I had
made in egglog, the best path forward is to question and test our understanding
as a whole.

## False confidence ##

The properties we've written so far give us some confidence that our definitions
make sense, and our theory of their behaviour holds up, since falsify was unable
to disprove anything with a counterexample. However, our confidence shouldn't be
*too* high, since most of those properties have two branches: the situation we
care about (which may be quite rare), and a fallback which is trivially
`True`{.haskell}.

For example, consider `agreeOnExtensionalInputs (n, x, y, inputs)`{.haskell},
which asserts that whenever `extensionalInputs`{.haskell} claims `x`{.haskell}
and `y`{.haskell} are extensionally equal for `i`{.haskell} symbolic inputs,
they agree on any sequence of `i`{.haskell} concrete input values. This
assertion has many caveats:

 - Unless `x`{.haskell} and `y`{.haskell} are extensioanlly equal, or happen to
   match a form that `provablyDisagree`{.haskell} can recognise, then
   `extensionalInputs x y`{.haskell} will never finish. In that case we are
   merely asserting the fallback of `runDelayOr True`{.haskell}, not learning
   anything about extensional equality.
 - In those cases that `extensionalInputs x y`{.haskell} *would* finish, it must
   take no more than `n`{.haskell} steps to avoid that fallback.
 - When `x`{.haskell} and `y`{.haskell} `provablyDisagree`{.haskell},
   `extensionalInputs`{.haskell} gives `Nothing`{.haskell} and we're merely
   asserting the *other* fallback: `Now True`{.haskell}.
 - When `extensionalInputs`{.haskell} gives `Just 0`{.haskell}, `x`{.haskell}
   and `y`{.haskell} are `normalEq`{.haskell}, so our assertion overlaps with
   other properties we've already established.

None of these make the property *false*; but they weaken the evidence that it
provides. We can see this more clearly by "labelling" each situation, and having
`falsify` report statistics on how common each was. Unfortunately this requires
us to return more than just a `Bool`{.haskell}, so we need to leave the world of
simple predicates (and hence cross-framework compatibility). Every
property-checker provides its own alternative implementations for such features:
in `falsify` it's the `Property`{.haskell} type. Here's a pretty direct
translation of `agreeOnExtensionalInputs`{.haskell} as a `Property`{.haskell},
with each of the above cases labelled:

```{.haskell pipe="./show Main.hs"}
agreeOnExtensionalInputsStats :: Property ()
agreeOnExtensionalInputsStats g =
  testProperty "agreeOnExtensionalInputs" $ do
    (n, x, y, inputs) <- gen g
    let check Nothing  = Now (Left "Not equal")
        check (Just i) = Right . (i,) . same <$> sAt i (agree x y inputs)
    label "Identical" [show (x == y)]
    case runDelayOr (Left "Timeout") (extensionalInputs x y >>= check) n of
      Left msg         -> label "result" [msg]
      Right (i, True ) -> label "i" [show i] *> label "result" ["True"]
      Right (i, False) -> fail ("Disagree on " ++ show (sTake i inputs))
```

```{.unwrap pipe="./run"}
main = check (agreeOnExtensionalInputsStats
               (tuple4 genFuel genCom genCom genComs))
```

The results will vary depending on the random seed (which changes every time
this page gets rendered), but I've seen around ⅓ resulting in `Timeout`, around
⅗ with `Not equal`, and only a handful resulting in `True`. Most of the latter
already agreed on 0 inputs, making them `normalEq`{.haskell} and hence avoiding
any need for symbolic execution. Thankfully `x`{.haskell} and `y`{.haskell} were
hardly ever identical, which would be even less interesting!

<details class="odd">
<summary>Similar stats for other properties…</summary>

```{.haskell pipe="./show Main.hs"}
notUnnormalEqToItselfStats g =
  testProperty "notUnnormalEqToItself" $ do
    (n, x) <- gen g
    case runDelayOr Nothing (Just <$> normalEq x x) n of
      Nothing           -> label "result" ["Timeout"]
      Just (Same _)     -> label "result" ["True"   ]
      Just (Diff x' y') -> fail (show x' ++ " =/= " ++ show y')

normalEqImpliesAgreeStats g =
  testProperty "normalEqImpliesAgree" $ do
    (n, f, g, xs) <- gen g
    case runDelay n (tuple2 (normalEq f g) (sHead (agree f g xs))) of
      Now got@(Same _  , Diff _ _) -> fail (show got)
      Now     (Same _  , Same _  ) -> label "result" ["Same Same"]
      Now     (Diff _ _, Same _  ) -> label "result" ["Diff Same"]
      Now     (Diff _ _, Diff _ _) -> label "result" ["Diff Diff"]
      Later   _                    -> label "result" ["Timeout"  ]

skNeverDisagreesWithSKSKKKStats g =
  testProperty "skNeverDisagreesWithSKSKKK" $ do
    (n, xs) <- gen g
    let f = App s k
        g = App (App s (App k (App s k))) (App k k)
    case runDelayOr Nothing (Just <$> sAt (2 + n) (agree f g xs)) n of
      Nothing             -> label "result" ["Timeout"]
      Just     (Same _  ) -> label "result" ["True"]
      Just got@(Diff _ _) -> fail (show got)

agreementIsMonotonicStats g =
  testProperty "agreementIsMonotonic" $ do
    ((n, m), (f, g), xs) <- gen g
    case runDelay n (tuple2 (sAt  n      (agree f g xs))
                            (sAt (n + m) (agree f g xs))) of
      Now got@(Same _  , Diff _ _) -> fail (show got)
      Now     (Same _  , Same _  ) -> label "result" ["Same Same"]
      Now     (Diff _ _, Same _  ) -> label "result" ["Diff Same"]
      Now     (Diff _ _, Diff _ _) -> label "result" ["Diff Diff"]
      Later   _                    -> label "result" ["Timeout"  ]

normalEqImpliesEverAgreeStats g =
  testProperty "normalEqImpliesEverAgree" $ do
    (n, x, y) <- gen g
    let go d = runDelayOr Nothing (Just <$> d) n
    case (go (same <$> normalEq x y), go (everAgree x y)) of
      (Nothing   , _         ) -> label "result" ["Timeout"]
      (Just False, _         ) -> label "result" ["Unequal"]
      (Just True , Just True ) -> label "result" ["True"   ]
      (Just True , Nothing   ) -> fail "everAgree timed out"
      (Just True , Just False) -> fail "Didn't agree"

commuteStats f g = do
    (x, y) <- gen g
    case (f x y, f y x) of
      (True , True ) -> label "result" ["Both"   ]
      (False, False) -> label "result" ["Neither"]
      (True , False) -> fail "f(x, y) but not f(y, x)"
      (False, True ) -> fail "f(y, x) but not f(x, y)"


distinctSymbolicHeadsCommutesStats =
  testProperty "distinctSymbolicHeadsCommutes" .
    commuteStats distinctSymbolicHeads

unequalArgCountCommutesStats =
  testProperty "unequalArgCountCommutes" .
    commuteStats unequalArgCount

provablyDisagreeCommutesStats =
  testProperty "provablyDisagreeCommutes" .
    commuteStats provablyDisagree

symbolGivenUnequalArgsCommutesStats g =
  testProperty "symbolGivenUnequalArgsCommutes" $ do
    (f, x, y) <- gen g
    commuteStats (liftFun2 symbolGivenUnequalArgs f) (pure (x, y))

extEqGeneralisesEqAndNormalEqAndEverAgreeStats g =
  testProperty "extEqGeneralisesEqAndNormalEqAndEverAgree" $ do
    (n, x, y) <- gen g
    let go l d = case runDelay n d of
          Now   b -> label l [show b   ] >> pure (Just b)
          Later _ -> label l ["Timeout"] >> pure Nothing
    ext <- go     "extEq" (            extEq x y)
    evr <- go "everAgree" (        everAgree x y)
    nml <- go  "normalEq" (same <$> normalEq x y)
    eql <- go     "equal" (pure  $      (==) x y)
    case ext of
      Nothing    -> pure ()
      Just True  -> label "result" ["Equal"]
      Just False -> case (evr, nml, eql) of
        (Just True, _        , _        ) -> fail "everAgree but not extEq"
        (_        , Just True, _        ) -> fail  "normalEq but not extEq"
        (_        , _        , Just True) -> fail        "== but not extEq"
        (_        , _        , _        ) -> label "result" ["Unequal"]
```

```{.unwrap pipe="NAME=stats ./run"}
main = defaultMain $ testGroup "Stats"
  [ notUnnormalEqToItselfStats
    (tuple2 genFuel genCom)

  , normalEqImpliesAgreeStats
    (tuple4 genFuel genCom genCom genComs)

  , skNeverDisagreesWithSKSKKKStats
    (tuple2 genFuel genComs)

  , agreementIsMonotonicStats
    (tuple3 (tuple2 genFuel genFuel) (tuple2 genCom genCom) genComs)

  , normalEqImpliesEverAgreeStats
    (tuple3 genFuel genCom genCom)

  , distinctSymbolicHeadsCommutesStats
    (tuple2 genSymCom genSymCom)

  , unequalArgCountCommutesStats
    (tuple2 genSymCom genSymCom)

  , symbolGivenUnequalArgsCommutesStats
    (tuple3 (Gen.fun (Gen.bool False)) genSymCom genSymCom)

  , provablyDisagreeCommutesStats
    (tuple2 genSymCom genSymCom)

  , agreeOnExtensionalInputsStats
    (tuple4 genFuel genCom genCom genComs)

  , extEqGeneralisesEqAndNormalEqAndEverAgreeStats
    (tuple3 genFuel genCom genCom)
  ]
```

</details>

TODO: Describe results

TODO: Abandon reduction if it gets too big (e.g. > 100 nodes)?

TODO: Measure size of terms?

TODO: Use smart generators to get better stats

```
{.haskell pipe="./show Main.hs"}
-- | Asserts that the first two Com values give equal results when applied to
-- | the third. First argument limits the number of steps attempted.
assertEqualOn n f g x = case runDelay n (normalEq fx gx) of
  Now (Right (fx', gx')) -> testFailed
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
    case runDelay steps (normalEq (App f v) (App g v)) of
      -- When f and g are distinct and agree for v, try them on another input
      Now (Left _) | f /= g -> do
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
  c <- genComN n                  -- Generate a Com value c
  runDelayOr (genNormalN n)       -- Fall back to generating a fresh Com
             (pure <$> reduce c)  -- Try to reduce c to a Normal value
             n                    -- Give up after n steps

-- | Generates (relatively small) Normal values
genNormal :: Gen Normal
genNormal = genNormalN limit
```

```{.unwrap pipe="./run"}
main = checkPred normalsAreNormalEqToThemselves (tuple2 genFuel genNormal)
```


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
          case runDelay (2 * fuel) (reduce (App c v)) of
            -- If we don't hit a normal form, skip this Com and recurse
            Later _ -> go result n
            Now key ->
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