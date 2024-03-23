---
title: "SK logic in egglog: part 3, falsifying myself"
packages: [ 'ghcWithFalsify' ]
dependencies: [ 'blog/2024-03-17-sk_logic_in_egglog_2.md' ]
---

<style>
.pass { background: #00bb0022; }
.fail { background: #bb000022; }
</style>

```{pipe="cat > chop && chmod +x chop"}
#!/bin/sh
set -eu

# Iterates through lines of the "part 2" markdown: when $1 matches (via grep) we
# output the following lines, until the end of a code-fence. This lets us re-use
# the same definitions without copy/pasting.
sed -n "/$1/,"' /^```$/p' < root/blog/2024-03-17-sk_logic_in_egglog_2.md |
  grep -v '^```'
```

```{pipe="sh"}
# Grab the Main.hs contents, as well as the scripts to append-to and run it
./chop 'show Main.hs' > Main.hs
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

```{.haskell pipe="./show Main.hs"}
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

-- | Huet Zipper, focused on the nth element of a 'Stream'.
sZipper :: Integral n => n -> Stream a -> ([a], Stream a)
sZipper = go []
  where go acc n         xs | n <= 0 = (acc, xs)
        go acc n (Cons x xs)         = go (x:acc) (n - 1) xs

sTake :: Natural -> Stream a -> [a]
sTake n = reverse . fst . sZipper n
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

```{.haskell pipe="./show Main.hs"}
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

Property-checkers, including `falsify`, allow tests to be "discarded": this
aborts the current call, and another example input is tried instead. Discarded
calls do not count towards the total (100 required successes, by default), so
they result in more calls being made; which slows down the test suite. If the
number of discarded calls reaches a predefined limit (100 in a row, by default)
then the test is abandoned as a failure: this is useful to know when our
precondition is too rare (or even impossible) to satisfy, and is preferable to
an infinite loop or a result with low statistical power. We'll refactor our
properties again (in the following fold-out section) to discard unwanted
branches:

<details class="odd">
<summary>Properties which discard…</summary>

```{.haskell pipe="./show Main.hs"}
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
`notUnnormalEqToItself`{.haskell} only hit a few timeouts, which could be
avoided by making a handful of extra calls. Other tests struggled, needing over
1000 extra calls to reach the 100 passes they required. Some also breached the
limit of 100 discards in a row, and caused the overall test suite to fail.

## Smarter generators ##

One way to avoid excessive discards is to move logic into our data generators.
This isn't a magic bullet, but it can be useful if acceptable values are
reasonably common; if retrying a generator is faster than discarding a tests;
and for retrying *parts* of an input, rather than starting from scratch.

For example, the following generator only produces `Normal`{.haskell} values, by
running `reduce`{.haskell} on generated `Com`{.haskell} values. That's
undecidable from *within* a property, but actually quite easy in a generator,
since we're free to discard problematic values and try again:

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

```{.haskell pipe="./show Main.hs"}
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

```{.haskell pipe="./show Main.hs"}
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

Unlike `normalsAreNormalEqToThemselves`{.haskell}, which *relies* on
`genNormal`{.haskell} to avoid timeout counterexamples, this is *purely* an
optimisation. For that reason, I actually prefer to mix in a few inputs from the
original "dumb" generator; that way, I don't need to assume the smart generator
is completely covering the input space:

```{.unwrap pipe="NAME=normalEqImpliesAgree ./run"}
main = defaultMain . normalEqImpliesAgreeDiscard $ do
  (x, y) <- Gen.frequency
    [ (9, genNormalEq           )
    , (1, (tuple2 genCom genCom)) ]
  tuple4 genFuel (pure x) (pure y) genComs
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
