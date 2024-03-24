---
title: "SK logic in egglog: part 3, falsifying myself"
packages: [ 'ghcWithFalsify', 'timeout' ]
dependencies: [ 'blog/2024-03-17-sk_logic_in_egglog_2.md' ]
---

<style>
.pass { background: #00bb0022; }
.fail { background: #bb000022; }
</style>

```{pipe="cat > chop && chmod +x chop"}
#!/bin/sh
set -eu

# Grab lines from the "part 2" markdown: when $1 matches (via regex) we output
# the following lines, until the end of a code-fence. This lets us re-use the
# same definitions without copy/pasting.
sed -n "/$1/,"' /^```$/p' < root/blog/2024-03-17-sk_logic_in_egglog_2.md |
  grep -v '^```'
```

```{pipe="sh"}
# Grab the Main.hs contents, as well as the scripts to append-to and run it
./chop 'pipe=.*\..show' > Main.hs
./chop 'cat > name' > name && chmod +x name
./chop 'cat > show' > show && chmod +x show
./chop 'cat > run'  > run  && chmod +x run
./chop 'cat > fail' > fail && chmod +x fail
```

```{pipe="cat > import && chmod +x import"}
#!/bin/sh
set -eu

# Appends stdin to Main.hs, after the first line (module declaration)
{
  head -n1 < Main.hs
  echo
  cat
  echo
  tail -n+2 < Main.hs
} > temp.hs
rm -f Main.hs
mv temp.hs Main.hs
```

In [the previous part](/blog/2024-03-17-sk_logic_in_egglog_2.html) we learned
some background theory about SK logic, and built up our notion of equality from
simple identity (`==`{.haskell}), through equivalence of `Normal`{.haskell}
forms (`normalEq`{.haskell}) up to full extensional equality
(`extEq`{.haskell}). In this post we'll investigate these definitions more
thoroughly. Whilst the eventual aim is to find the mistake I'd made in egglog,
the best path forward is to question and test our understanding as a whole.

## False confidence ##

The properties we've written so far give us some confidence that our definitions
make sense, and our theory of their behaviour holds up, since falsify was unable
to disprove anything with a counterexample. However, our confidence shouldn't be
*too* high, since most of those properties have two branches: the situation we
care about, and a fallback which is trivially `True`{.haskell}. If the former is
quite rare, finding no counterexamples may not tell us very much.

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

```{.haskell pipe="./import"}
import Test.Falsify.Property (Property, discard, gen, label)
```

```{.haskell pipe="./show"}
agreeOnExtensionalInputsStats g =
  testProperty "agreeOnExtensionalInputs" $ do
    (n, x, y, ins) <- gen g
    let check Nothing  = Now (Left "Not equal")
        check (Just i) = Right . (i,) . same <$> sAt i (agree x y ins)
    label "Identical" [show (x == y)]
    case runDelayOr (Left "Timeout") (extensionalInputs x y >>= check) n of
      Left msg         -> label "result" [msg]
      Right (i, True ) -> label "i" [show i] *> label "result" ["True"]
      Right (i, False) -> fail ("Disagree on " ++ show (sTake i ins))

-- | Split the first 'n' elements off a 'Stream'.
sSplitAt :: Integral n => n -> Stream a -> ([a], Stream a)
sSplitAt = go []
  where go acc n         xs | n <= 0 = (reverse acc, xs)
        go acc n (Cons x xs)         = go (x:acc) (n - 1) xs

-- | Take the first 'n' elements of a 'Stream' as a list.
sTake :: Natural -> Stream a -> [a]
sTake n = fst . sSplitAt n
```

```{.unwrap pipe="NAME=_ ./run"}
main = defaultMain (agreeOnExtensionalInputsStats
                     (tuple4 genFuel genCom genCom genComs))
```

The results will vary depending on the random seed (which changes every time
this page gets rendered), but I've seen around ⅓ resulting in `Timeout`, around
⅗ with `Not equal`, and only a handful resulting in `True`. Most of the latter
agree on $0$ inputs, making them `normalEq`{.haskell} and hence bypassing the
use of symbolic execution. The only good news is that `x`{.haskell} and
`y`{.haskell} were hardly ever identical!

Here are similar results for other properties (their definitions are hidden in
the following fold-out section, since the logic hasn't changed):

<details class="odd">
<summary>Labelled properties…</summary>

```{.haskell pipe="./show"}
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
    let go d = runDelayOr Nothing (Just <$> d)
    case (go (same <$> normalEq x y) n, go (everAgree x y) (triangle n)) of
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
      Nothing    -> label "result" ["Timeout"]
      Just True  -> label "result" ["Equal"  ]
      Just False -> case (evr, nml, eql) of
        (Just True, _        , _        ) -> fail "everAgree but not extEq"
        (_        , Just True, _        ) -> fail  "normalEq but not extEq"
        (_        , _        , Just True) -> fail        "== but not extEq"
        (_        , _        , _        ) -> label "result" ["Unequal"]
```

</details>

```{.haskell pipe="./import"}
import Test.Tasty (testGroup)
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

Again, the exact distribution of these tests will vary from run to run; but I've
seen some that *always* time-out, or never satisfy the required preconditions,
etc. Sometimes this is a legitimate problem with our logic: for example, the
properties `skNeverDisagreesWithSKSKKKStats`{.haskell} and
`agreementIsMonotonic`{.haskell} always hit a timeout, since they are using the
same `Fuel`{.haskell} parameter as the number of inputs and number of steps
before timing out. We can avoid this by using separate parameters for each:

```{.haskell pipe="./show"}
skNeverDisagreesWithSKSKKKFixed g =
  testProperty "skNeverDisagreesWithSKSKKK" $ do
    (n, m, xs) <- gen g
    let f = App s k
        g = App (App s (App k (App s k))) (App k k)
    case runDelayOr Nothing (Just <$> sAt (2 + n) (agree f g xs)) (m + n) of
      Nothing             -> label "result" ["Timeout"]
      Just     (Same _  ) -> label "result" ["True"   ]
      Just got@(Diff _ _) -> fail (show got)

agreementIsMonotonicFixed g =
  testProperty "agreementIsMonotonic" $ do
    ((i1, i2), n, (f, g), xs) <- gen g
    case tuple2 (runDelay n (sAt  i1       (agree f g xs)))
                (runDelay n (sAt (i1 + i2) (agree f g xs))) of
      Now got@(Same _  , Diff _ _) -> fail (show got)
      Now     (Same _  , Same _  ) -> label "result" ["Same Same"]
      Now     (Diff _ _, Same _  ) -> label "result" ["Diff Same"]
      Now     (Diff _ _, Diff _ _) -> label "result" ["Diff Diff"]
      Later   _                    -> label "result" ["Timeout"  ]
```

```{.unwrap pipe="NAME=skNeverDisagreesWithSKSKKK ./run"}
main = defaultMain (skNeverDisagreesWithSKSKKKFixed
                     (tuple3 genFuel genFuel genComs))
```

```{.unwrap pipe="NAME=agreementIsMonotonic ./run"}
main = defaultMain (agreementIsMonotonicFixed
                     (tuple4 (tuple2 genFuel genFuel)
                             genFuel
                             (tuple2 genCom genCom)
                             genComs))
```

## Discarding uninteresting cases ##

Property-checkers, including `falsify`, allow us to "discard" uninteresting
cases, which aborts the current call generates another input to check instead.
Discarded calls do not count towards the reported total, so they avoid the false
confidence issue we saw above. The downside is that extra calls make the test
slower; and it will be abandoned entirely if too many are discarded (for
`falsify`, the default limit is 100 in a row). Depending on the property checker
that may be considered a test failure: `falsify` only considers abandoned tests
to have failed if there were *no* successful calls.

In the following fold-out section we refactor our properties again, to discard
any branches that are "uninteresting", like timeouts:

<details class="odd">
<summary>Properties which discard…</summary>

```{.haskell pipe="./show"}
notUnnormalEqToItselfDiscard g =
  testProperty "notUnnormalEqToItself" $ do
    (n, x) <- gen g
    case runDelayOr Nothing (Just <$> normalEq x x) n of
      Nothing           -> discard
      Just (Same _)     -> pure ()
      Just (Diff x' y') -> fail (show x' ++ " =/= " ++ show y')

normalEqImpliesAgreeDiscard g =
  testProperty "normalEqImpliesAgree" $ do
    (n, f, g, xs) <- gen g
    case runDelay n (tuple2 (normalEq f g) (sHead (agree f g xs))) of
      Now got@(Same _  , Diff _ _) -> fail (show got)
      Now     (Same _  , Same _  ) -> pure ()
      Now     (Diff _ _, _       ) -> discard
      Later   _                    -> discard

skNeverDisagreesWithSKSKKKDiscard g =
  testProperty "skNeverDisagreesWithSKSKKK" $ do
    (n, m, xs) <- gen g
    let f = App s k
        g = App (App s (App k (App s k))) (App k k)
    case runDelayOr Nothing (Just <$> sAt (2 + n) (agree f g xs)) (m + n) of
      Nothing             -> discard
      Just     (Same _  ) -> pure ()
      Just got@(Diff _ _) -> fail (show got)

agreementIsMonotonicDiscard g =
  testProperty "agreementIsMonotonic" $ do
    ((i1, i2), n, (f, g), xs) <- gen g
    case tuple2 (runDelay n (sAt  i1       (agree f g xs)))
                (runDelay n (sAt (i1 + i2) (agree f g xs))) of
      Now got@(Same _  , Diff _ _) -> fail (show got)
      Now     (Same _  , Same _  ) -> pure ()
      Now     (Diff _ _, _       ) -> discard
      Later   _                    -> discard

normalEqImpliesEverAgreeDiscard g =
  testProperty "normalEqImpliesEverAgree" $ do
    (n, x, y) <- gen g
    let go d = runDelayOr Nothing (Just <$> d)
    case (go (same <$> normalEq x y) n, go (everAgree x y) (triangle n)) of
      (Nothing   , _         ) -> discard
      (Just False, _         ) -> discard
      (Just True , Just True ) -> pure ()
      (Just True , Nothing   ) -> fail "everAgree timed out"
      (Just True , Just False) -> fail "Didn't agree"

agreeOnExtensionalInputsDiscard g =
  testProperty "agreeOnExtensionalInputs" $ do
    (n, x, y, ins) <- gen g
    let check Nothing  = pure discard
        check (Just i) = pure . (i,) . same <$> sAt i (agree x y ins)
    if x == y
       then discard
       else do (i, b) <- runDelayOr discard (extensionalInputs x y >>= check) n
               label "i" [show i]
               if b
                  then pure ()
                  else fail ("Disagree on " ++ show (sTake i ins))

extEqGeneralisesEqAndNormalEqAndEverAgreeDiscard g =
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
      Nothing    -> discard
      Just True  -> pure ()
      Just False -> case (evr, nml, eql) of
        (Just True, _        , _        ) -> fail "everAgree but not extEq"
        (_        , Just True, _        ) -> fail  "normalEq but not extEq"
        (_        , _        , Just True) -> fail        "== but not extEq"
        (_        , _        , _        ) -> discard
```

</details>

```{.unwrap pipe="NAME=discard ./fail"}
main = defaultMain $ testGroup "Discard"
  [ notUnnormalEqToItselfDiscard
    (tuple2 genFuel genCom)

  , normalEqImpliesAgreeDiscard
    (tuple4 genFuel genCom genCom genComs)

  , skNeverDisagreesWithSKSKKKDiscard
    (tuple3 genFuel genFuel genComs)

  , agreementIsMonotonicDiscard
    (tuple4 (tuple2 genFuel genFuel) genFuel (tuple2 genCom genCom) genComs)

  , normalEqImpliesEverAgreeDiscard
    (tuple3 genFuel genCom genCom)

  , agreeOnExtensionalInputsDiscard
    (tuple4 genFuel genCom genCom genComs)

  , extEqGeneralisesEqAndNormalEqAndEverAgreeDiscard
    (tuple3 genFuel genCom genCom)
  ]
```

Discarding works very well for some tests, e.g.
`notUnnormalEqToItself`{.haskell} only hit a few timeouts, which caused a
handful of extra calls. Other tests struggled, making over 1000 extra calls;
some were abandoned after reaching the limit of 100 discards in a row; and some
of those gave up without a single success, causing the overall test suite to
fail.

## Smarter generators ##

One way to avoid excessive discards is to move logic into our data generators.
This isn't a magic bullet, but it can be useful if acceptable values are
reasonably common; if retrying a generator is faster than discarding a test;
and for retrying *parts* of an input, rather than starting from scratch.

For example, the following generator only produces `Normal`{.haskell} values, by
running `reduce`{.haskell} on generated `Com`{.haskell} values. That's
undecidable from *within* a property, but actually quite easy in a generator,
since we're free to discard problematic values and try again:

```{.haskell pipe="./show"}
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

-- | Generates a 'Stream' of 'Normal' values of the given size
genNormalsN :: Fuel -> Gen (Stream Normal)
genNormalsN n = Cons <$> genNormalN n <*> genNormalsN n

-- | Generates a 'Stream' of reasonably-sized 'Normal' values
genNormals :: Gen (Stream Normal)
genNormals = genNormalsN limit
```

Since `Normal`{.haskell} values reduce immediately, their `normalEq`{.haskell}
result is always `Now`{.haskell}, and hence they can be compared with any amount
of `Fuel`{.haskell} without timing out. This fixes our very first property, that
all values are `normalEq`{.haskell} to themselves!

```{.haskell pipe="./show"}
normalsAreNormalEqToThemselves :: (Fuel, Normal) -> Bool
normalsAreNormalEqToThemselves (n, x) = normalEqToItself (n, toCom x)
```

```{.unwrap pipe="./run"}
main = check normalsAreNormalEqToThemselves (tuple2 genFuel genNormal)
```

The following generator makes pairs of values which are `normalEq`{.haskell} to
each other (but not identical). It's reasonably rare for independently-generated
values to match that way, which caused a lot of the discarding above. However,
our generator can be more efficient by accumulating values until *any* of them
match (exploiting the so-called
["birthday paradox"](https://en.wikipedia.org/wiki/Birthday_problem)):

```{.haskell pipe="./import"}
import Data.Set (Set)
import qualified Data.Set as Set
```

```{.haskell pipe="./show"}
-- | Accumulate more and more values from the given 'Gen', until we find two
-- | that satisfy the given relation. The 'i' parameter allows for flexibility.
genMatching :: Ord o => (i -> Gen o) -> (i -> o -> o -> Bool) -> i -> Gen (o, o)
genMatching g match i = go Set.empty  -- Begin with no known values
  where go xs = do
          -- Check if a freshly generated value matches any we know
          x <- g i
          let (matched, unmatched) = Set.partition (match i x) xs
          case Set.toList matched of
            y:_ -> pure (x, y)
            []  -> go (Set.insert x unmatched) -- Remember value & try again

-- | Generate pairs of unequal 'Com' values which have the same 'Normal' form.
genNormalEqN :: Fuel -> Gen (Com, Com)
genNormalEqN = genMatching genComN match
  where match n x y = x /= y && runDelayOr False (same <$> normalEq x y) n

genNormalEq :: Gen (Com, Com)
genNormalEq = genNormalEqN limit
```

This generator greatly reduces discards for a property like
`normalEqImpliesAgree`{.haskell}:

```{.unwrap pipe="NAME=normalEqImpliesAgree ./run"}
main = defaultMain . normalEqImpliesAgreeDiscard $ do
  (x, y) <- genNormalEq
  tuple4 genFuel (pure x) (pure y) genComs
```

### Hedging our bets ###

Unlike `normalsAreNormalEqToThemselves`{.haskell}, which *relies* on
`genNormal`{.haskell} to avoid timeout counterexamples, the smart generator for
`normalEqImpliesAgree`{.haskell} is *purely* an optimisation. For that reason, I
actually prefer to mix in a few inputs from the original "dumb" generator: that
way, we don't need to trust the smart generator to completely cover the input
space; and we leave open the possibility for the dumb generator to stumble on to
a different form of counterexample. The simplest way to mix two generators is
via the `Gen.choose`{.haskell} function, which uses each half of the time; but
that would increase the number of discards more than I would like. Instead we'll
use the more sophisticated `Gen.frequency`{.haskell}:

```{.unwrap pipe="NAME=normalEqImpliesAgree ./run"}
main = defaultMain . normalEqImpliesAgreeDiscard $ do
  (x, y) <- Gen.frequency
    [ (9, genNormalEq           )
    , (1, (tuple2 genCom genCom)) ]
  tuple4 genFuel (pure x) (pure y) genComs
```

### A trickier case ###

We can use a similar approach for `agreementIsMonotonic`{.haskell}, generating
a pair of values which only agree on a non-zero number of inputs:

```{.haskell pipe="./show"}
-- | Generate a pair of 'Com' values which agree on the given number of inputs,
-- | within the given amount of 'Fuel'.
genAgreeFromN :: Natural -> Fuel -> Gen (Com, Com)
genAgreeFromN lo = genMatching genComN match
  where match n x y = case runDelayOr Nothing (extensionalInputs x y) n of
          Just i  -> i >= lo
          Nothing -> False

-- | Generate a pair of 'Com' values which agree on 1 or more inputs. This tends
-- | to avoid values which agree on 0 inputs, i.e. with equal 'Normal' forms,
-- | and hence exercise the symbolic execution more thoroughly.
genAgreeN = genAgreeFromN 1

-- | Generate pairs of reasonably sized values which agree on 1 or more inputs.
genAgree = genAgreeN limit
```

```{.unwrap pipe="NAME=agreementIsMonotonic ./run"}
main = defaultMain . agreementIsMonotonicDiscard . Gen.frequency $
    [(9, smart), (1, dumb)]
  where dumb  = tuple4 (tuple2 genNat genNat)
                       genFuel
                       (tuple2 genCom genCom)
                       genComs
        smart = do
          n      <- (10 +) <$> genFuel
          (x, y) <- genAgreeN n  -- Generate values that agree on some inputs
          case runDelay n (extensionalInputs x y) of  -- Find how many inputs
            Now (Just i) -> tuple4
              (tuple2 (pure i) genNat)
              (pure n)
              (pure (x, y))
              (fmap toCom <$> genNormals)  -- Normal values don't need any Fuel
            _ -> smart  -- absurd, but retry as a fallback
        genNat = fromIntegral <$> genFuel
```

This test now takes a lot longer to run than the others, due to the more precise
precondition we require of the generator. I see that as a good investment, since
we're now maxing-out our CPU in a (reasonably targeted) attempt to find mistakes
in our reasoning; which seems preferable to the original test, which quickly
gave up even trying. Since the number of runs is configurable, we can choose a
low number for a fast feedback cycle during development, and crank it up for a
more thorough check like a pre-push git hook or a continuous integration server.

## Thoroughly checking extensionality ##

Most of the checks so far have been focused on implementation details, like the
definition of "agreement" and the particular patterns of disagreement we're
looking for. Now I want to focus on extensional equality itself, and its
implications.

### Extensionally equal values are indistinguishable ###

The main reason we care about extensional equality is that it's broad enough to
relate all values which are indistinguishable *within* SK. This is also known as
[observational
equivalence](https://en.wikipedia.org/wiki/Observational_equivalence) (SK is
such a simple language that these notions end up coinciding!).

As a consequence, extensionally equal values are interchangable: swapping any
part of an SK expression for an extensionally-equal alternative should make no
observable difference to the result; i.e. the results should themselves be
extensionally equal.

This is difficult to test: the double-negative of being "not unequal" doesn't
give us much confidence, since we can only prove inequality in certain specific
cases; yet we don't know how many inputs it may take to prove equality, and
hence how much `Fuel`{.haskell} would be needed.

We'll work around this by *removing* the timeout: this is a bold move, since our
test suite may run forever if we're wrong!

```{.haskell pipe="./show"}
-- | Runs a 'Delay' value to completion. This may run forever!
unsafeRunDelay :: Delay a -> a
unsafeRunDelay (Now   x) = x
unsafeRunDelay (Later x) = unsafeRunDelay x
```

To represent "swapping any part", we'll use a
[zipper datastructure](https://en.wikipedia.org/wiki/Zipper_(data_structure)):

```{.haskell pipe="./show"}
-- | Represents a 'Com' with one of its sub-expressions "focused"
type ComZipper = (Com, [Either Com Com])

-- | Turns a 'ComZipper' back into a 'Com'
unzipCom :: ComZipper -> Com
unzipCom (x, xs) = case xs of
  []         -> x
  Left  r:ys -> unzipCom (App x r, ys)
  Right l:ys -> unzipCom (App l x, ys)

-- | A path down a binary tree
type Path = [Either () ()]

-- | Focus on a particular sub-expression of the given 'Com', at a position
-- | identified by the given 'Path' (stopping if it hits a leaf).
focus :: Com -> Path -> ComZipper
focus x = go (x      , [])
  where   go (App l r, xs) ( Left ():p) = go (l,  Left r:xs) p
          go (App l r, xs) (Right ():p) = go (r, Right l:xs) p
          go z             _            = z

-- | Generate a 'Path' through a binary tree (e.g. 'Com').
genPathN :: Fuel -> Gen Path
genPathN n = Gen.list (to n) (go <$> Gen.bool False)
  where go False =  Left ()
        go True  = Right ()

-- | Generate a reasonably small 'Path' through a binary tree (e.g. 'Com')
genPath :: Gen Path
genPath = genPathN limit
```

<details class="odd">
<summary>Checking `ComZipper`{.haskell}…</summary>

We'd better do some sanity checks on `ComZipper`{.haskell}, to make sure it
behaves as we expect:

```{.haskell pipe="./show"}
unzipComIgnoresLocation = testProperty "unzipComIgnoresLocation" $ do
  c <- gen genCom
  p <- gen genPath
  let c' = unzipCom (focus c p)
  if c == c' then pure () else fail (show (c, c'))
```

```{.unwrap pipe="NAME=unzipComIgnoresLocation ./run"}
main = defaultMain unzipComIgnoresLocation
```

</details>

```{.haskell pipe="./import"}
import Data.Default (def)
import Test.Tasty.Falsify (overrideNumTests, testPropertyWith)
```

```{.haskell pipe="./show"}
extEqAreInterchangable = testProperty "extEqAreInterchangable" $ do
  -- Generate an expression in Normal form (to avoid infinite loops); focus on
  -- an arbitrary Path inside it, and discard that focus (creating a "hole").
  (_, zs) <- gen $ focus . toCom <$> genNormal <*> genPath
  -- Generate extensionally-equal expressions, to plug that hole
  (x, y ) <- gen genAgree
  let (x', y') = (unzipCom (x, zs), unzipCom (y, zs))
  if unsafeRunDelay (extEq x' y') then pure () else fail (show (x', y'))
```

```{.unwrap pipe="sh"}
export NAME=extEqAreInterchangable
export MAX_SECS=300
CODE='main = defaultMain extEqAreInterchangable'
for RETRY in $(seq 1 10)
do
   if GOT=$(echo "$CODE" | withTimeout ./run) 2> er
   then
     cat er 1>&2
     echo "$GOT"
     exit 0
   else
     echo "Retrying $NAME $RETRY" 1>&2
   fi
done
cat er
exit 1
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
