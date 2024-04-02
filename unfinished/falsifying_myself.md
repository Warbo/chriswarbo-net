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

```{.haskell pipe="./import"}
import Test.Falsify.Generator (shrinkWith)
```

```{.haskell pipe="./show"}
-- | Like genComN, but reduces its outputs to normal form. The fuel
-- | bounds the size of the initial expression (before it's reduced), and
-- | the number of steps to attempt when normalising.
genNormalN :: Fuel -> Gen Normal
genNormalN n = shrinkWith shrinkNormal $ do
  c <- genComN n                  -- Generate a Com value c
  runDelayOr (genNormalN n)       -- Fall back to generating a fresh Com
             (pure <$> reduce c)  -- Try to reduce c to a Normal value
             n                    -- Give up after n steps

-- | Generates (relatively small) Normal values
genNormal :: Gen Normal
genNormal = genNormalN limit

-- | Generates a 'Stream' from the given generator
genStream :: Gen a -> Gen (Stream a)
genStream g = Cons <$> g <*> genStream g

-- | Generates a 'Stream' of 'Normal' values of the given size
genNormalsN :: Fuel -> Gen (Stream Normal)
genNormalsN = genStream . genNormalN

-- | Generates a 'Stream' of reasonably-sized 'Normal' values
genNormals :: Gen (Stream Normal)
genNormals = genNormalsN limit
```

### A minor digression about shrinking ###

Property-checkers which generate inputs randomly, like `falsify`, may stumble
across monstrous counterexamples full of irrelevant structure that is tedious
for users to tease apart and find the underlying problem. To reduce this burden,
all such checkers will attempt to "shrink" the counterexamples they find, in the
hope of discarding irrelevant parts: when a counterexample is found, a
"shrinker" turns it into a list of possible alternatives. If that list is empty,
shrinking stops and the counterexample is shown the the user. If the list is not
empty, the property is retried with the first alternative: if the property holds
then the next alternative is tried, and so on. If an alternative is found for
which the property *does not* hold, then we've found a smaller counterexample.
The shrinking process is repeated for this smaller counterexample, and so on
until there are no alternatives remaining (either because the counterexample
cannot be shrunk, like an empty list; or the property held for every smaller
alternative).

There is no precise definition for what "shrinking" means, other than a
requirement to avoid cycles: if `foo` can be shrunk to `bar`, then `bar` should
not also shrink to `foo`, or else the above algorithm can get stuck in a loop.

[QuickCheck](https://hackage.haskell.org/package/QuickCheck) implements
shrinking alongside, but separately, to its data generators. A QuickCheck-style
shrinker is a value with the following type:

```{.haskell pipe="./show"}
type Shrink a = a -> [a]
```

That is, a function which takes as input the value we want to shrink, and
outputs a list (potentially empty!) of "smaller" alternatives. For example, we
could shrink a `Com`{.haskell} value in several ways:

 - Replace leaf values with `K` (the simpler of the two basis values). We can't
   replace `K` with itself, since that's a cycle, so it has no alternatives.
 - Replace branches (`App` values) with `S` or `K`, since leaves are smaller.
 - Replace branches with either of their children.
 - Shrink either of a branch's children.

```{.haskell pipe="./show"}
shrinkCom :: Shrink Com
shrinkCom (C   "K") = []
shrinkCom (C     _) = [k]
shrinkCom (App l r) = interleave
  [ [ k, s ]
  , filter (`notElem` [k, s]) [ l, r ]  -- Avoid duplicating k or s
  ,      App l <$> shrinkCom r
  , flip App r <$> shrinkCom l
  ]

-- | Alternate taking elements from each list.
interleave :: [[a]] -> [a]
interleave ((x:xs):ys) = x : interleave (ys ++ [xs])
interleave ([]    :ys) =     interleave  ys
interleave  []         = []
```

The `shrinkNormal`{.haskell} function we reference above is a simple wrapper
around `shrinkCom`{.haskell}, which discards non-`Normal`{.haskell} values:

```{.haskell pipe="./show"}
shrinkNormal :: Shrink Normal
shrinkNormal = keep . shrinkCom . toCom
  where keep []     = []
        keep (c:cs) = case toNormal c of
          Left  _ ->     keep cs
          Right n -> n : keep cs
```

We use `shrinkWith shrinkNormal`{.haskell} in our definition of
`genNormalN`{.haskell} so that the shrinking algorithm described above will get
smaller alternatives by calling our `shrinkNormal`{.haskell} function. That
would be the norm in QuickCheck (or its descendents, like ScalaCheck), but
`falsify` is a bit smarter: it doesn't need custom shrinking functions, since
its able to re-run its generators on smaller (trees of) random numbers. Such an
"integrated shrinking" approach is *usually* good enough, and saves us a bunch
of effort (after all, we've got this far without having to know about
shrinking!). Unfortunately, integrated shrinking doesn't work well for smarter
generators that perform a search, like `genNormalN`{.haskell}: there's no reason
to expect that re-running the search with some smaller random numbers will find
a smaller result. Indeed it may find *no* result, and get stuck recursing
forever. When writing such "smart" generators, it's hence worth thinking about
their shrinking behaviour, and whether it would be prudent to override.

Here are a few more general-purpose shrinkers (in a [scrap your type
classes](https://www.haskellforall.com/2012/05/scrap-your-type-classes.html)
style):

```{.haskell pipe="./show"}
-- | Shrink the elements of a tuple.
shrink2 :: Shrink a -> Shrink b -> Shrink (a, b)
shrink2 sA sB (x, y) = interleave [(,y) <$> sA x, (x,) <$> sB y]

-- | Shrink the elements of a tuple.
shrink3 :: Shrink a -> Shrink b -> Shrink c -> Shrink (a, b, c)
shrink3 sA sB sC (x, y, z) = unpack <$> shrink2 sA (shrink2 sB sC) (x, (y, z))
  where unpack (x', (y', z')) = (x', y', z')

-- | Shrink a list, by dropping and shrinking its elementsthe elements of a tuple.
shrinkL :: Shrink a -> Shrink [a]
shrinkL _  []  = []
shrinkL sA [x] = [] : (pure <$> sA x)
shrinkL sA xs  = [] : shrinkElems
  where len         = length xs
        shrinkElems = do
          i <- [0..len-1]
          case splitAt i xs of
            -- Try dropping or shrinking the ith element
            (pre, a:suf) -> (pre ++ suf) : ((pre ++) . (:suf) <$> sA a)
            _            -> []
```

### Revisiting our roots ###

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

### Generating related values ###

The following generator makes a `Set`{.haskell} of values which satisfy some
given binary predicate. The most straightforward way to implement this would be
generating sets of values over and over until we eventually find one that fits
our criteria. However, it may be quite rare for values to satisfy that predicate
and attempting to generate *multiple* at once will just compound that rarity.

Instead, our "smart" generator can be more efficient by accumulating values
until *any combination* of them satisfies our criteria (exploiting the so-called
["birthday paradox"](https://en.wikipedia.org/wiki/Birthday_problem)):

```{.haskell pipe="./import"}
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict as Map
import           Data.Set          (Set)
import qualified Data.Set        as Set
```

```{.haskell pipe="./show"}
-- | Accumulate more and more values from the given 'Gen', until we find 'n'
-- | that satisfy the given relation. The 'i' parameter allows for flexibility.
genMatchingN :: Ord o
             => Natural
             -> (i -> Gen o)
             -> (i -> o -> o -> Bool)
             -> Shrink o
             -> i
             -> Gen (Set o)
genMatchingN n g match shr i = shrinkWith shrink (go Map.empty)
  where go xss = do
          x <- g i
          let k    = case filter (match i x) (Map.keys xss) of
                       y:_ -> y
                       _   -> x
              -- Append x to the Set which matches k (or empty, if not present)
              xs   = Map.findWithDefault Set.empty k xss
          case Set.insert x xs of
            xs -> if len xs >= n
                     then pure xs
                     else go (Map.insert k xs xss)

        -- Try shrinking the elements individually, keeping any that still match
        shrink =
          filter ((== n) . len) . (Set.fromList <$>) . shrink' . Set.toList

        shrink' []     = []
        shrink' (x:xs) =
          keep (interleave [(:xs) <$> shr x, (x:) <$> shrink' xs])

        len = fromIntegral . Set.size

        keep []           = []
        keep (    []:xss) = keep xss
        keep ((x:xs):xss) =
          (if all (match i x) xs then [x:xs] else []) ++ keep xss

-- | Accumulate more and more values from the given 'Gen', until we find two
-- | that satisfy the given relation. The 'i' parameter allows for flexibility.
genMatching :: Ord o
            => (i -> Gen o)
            -> (i -> o -> o -> Bool)
            -> Shrink o
            -> i
            -> Gen (o, o)
genMatching g match shr i = genMatchingN 2 g match shr i >>= get
  where get s = case Set.toList s of
          x:y:_ -> pure (x, y)
          _     -> genMatching g match shr i -- absurd but fall back to retrying

-- | Generate pairs of unequal 'Com' values which have the same 'Normal' form.
genNormalEqN :: Fuel -> Gen (Com, Com)
genNormalEqN = genMatching genComN matchNormalEqN shrinkCom

matchNormalEqN n x y = x /= y && runDelayOr False (same <$> normalEq x y) n

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
genAgreeFromN lo = genMatching genComN (matchAgreeFromN lo) shrinkCom

matchAgreeFromN lo n x y = case runDelayOr Nothing (extensionalInputs x y) n of
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
extensionally equal. We can represent such "swapping" using a
[zipper datastructure](https://en.wikipedia.org/wiki/Zipper_(data_structure)):

```{.haskell pipe="./show"}
-- | A path down a binary tree
type Path = [Either () ()]

-- | Replace a part (identified by 'Path') of the first 'Com' with the second.
swapPart :: Path -> Com -> Com -> Com
swapPart p whole part = unzipCom (part, position)
  where (_, position) = focus whole p

-- | Represents a 'Com' with one of its sub-expressions "focused"
type ComZipper = (Com, [Either Com Com])

-- | Turns a 'ComZipper' back into a 'Com'
unzipCom :: ComZipper -> Com
unzipCom (x, xs) = case xs of
  []         -> x                       -- x is the root, return it as-is
  Left  r:ys -> unzipCom (App x r, ys)  -- x is a left child with sibling r
  Right l:ys -> unzipCom (App l x, ys)  -- x is a right child with sibling l

-- | Focus on a particular sub-expression of the given 'Com', at a position
-- | identified by the given 'Path' (stopping if it hits a leaf).
focus :: Com -> Path -> ComZipper
focus x = go (x, [])  -- Start focused on the root
  where go (App l r, xs) ( Left ():p) = go (l,  Left r:xs) p  -- Focus on left
        go (App l r, xs) (Right ():p) = go (r, Right l:xs) p  -- Focus on right
        go z             _            = z  -- Reached a leaf or end of Path

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

We can sanity-check our `ComZipper`{.haskell} implementation using the property
that turning them back into a `Com`{.haskell} value doesn't depend on which part
was "focused":

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

Swapping-out part of an *arbitrary* expression, even one in `Normal`{.haskell}
form, may result in a value which loops forever. For example, consider a classic
infinite loop like `S(SKK)(SKK)(S(SKK)(SKK))`:

 - The expression `SKK` acts as an
   [identity function](https://en.wikipedia.org/wiki/Identity_function),
   reducing to its first input value: `SKKx → Kx(Kx) → x`.
 - Let's abbreviate `SKK` as `I`, so our loop can be written `SII(SII)`.
 - Applying the `S` rule, we get `SII(SII) → I(SII)(I(SII))`.
 - Since `Ix` reduces to `x`, both of those `I(SII)` values reduce to `SII`.
 - Hence `I(SII)(I(SII)) → SII(SII)`, which is what we started with!
 - Therefore our original expression `S(SKK)(SKK)(S(SKK)(SKK))` reduces to
   itself, over and over, forever.

Note that the repeated component `S(SKK)(SKK)` is itself in `Normal`{.haskell}
form, since each `S` is only applied to two args. Applying `K` to that, like
`K(S(SKK)(SKK))`, is also `Normal`{.haskell}. If we swap-out that `K` for
`S(SKK)(SKK)`, we'll get our infinite loop.

Undecidability makes it impossible, in general, to avoid creating such loops; so
we need our generator to retry if it can't normalise a swapped-out expression in
a reasonable number of steps:

```{.haskell pipe="./show"}
genSwappableExtEqValsN :: Fuel -> Gen (Com, Com, [Either Com Com])
genSwappableExtEqValsN n = shrinkWith zShrink $ do
    -- Generate a pair of extensionally-equal Coms. Avoid normally-equal values,
    -- since they don't need symbolic execution.
    (x, y) <- genMatching genNormalComN matchNontriviallyExtEq shrinkCom n
    -- Generate a ComZipper whose focus can be swapped-out with 'x' or 'y'
    -- without diverging
    zs <- genZipperFor x y
    pure (x, y, zs)
  where genZipperFor x y = do
          (_, zs) <- focus <$> genComN n <*> genPathN n
          if checkZs (x, y, zs) then pure zs else genZipperFor x y

        checkZs (x, y, zs) =
          let go = isJust . countLaters n . reduce . unzipCom . (, zs)
           in go x && go y

        zShrink = filter checkZs
                . shrink3 shrinkCom shrinkCom (shrinkL (const []))

-- | More efficient alternative to 'genComN': biased towards smaller values, and
-- | only generates 'Normal' forms.
genNormalComN = (toCom <$>) . genNormalN

-- | Whether two 'Com' values are 'extEq' but *not* 'normalEq' (within 'Fuel').
matchNontriviallyExtEq :: Fuel -> Com -> Com -> Bool
matchNontriviallyExtEq n x y = runDelayOr False (            extEq x y) n
                            && runDelayOr False (diff <$> normalEq x y) n
```

Now we can generate extensionally-equal values, an expression to plug them into,
and a `Path`{.haskell} describing where to plug them in, filtered such that they
both reduce to a `Normal`{.haskell}. However, one problem remains: we have no
idea how much `Fuel`{.haskell} will be needed to find when those swapped-out
expressions begin to agree. We can re-use our double-negative trick of being
"not unequal":

```{.haskell pipe="./show"}
extEqCannotBeDistinguished = testProperty "extEqCannotBeDistinguished" $ do
  -- Use the smart generator 90% of the time, allow 10% to be unconstrained
  (x, y, zs) <- gen $ Gen.frequency
    [ (9, genSwappableExtEqValsN limit)
    , (1, tuple3 genCom genCom (snd <$> (focus <$> genCom <*> genPath)))
    ]
  let (x', y') = (unzipCom (x, zs), unzipCom (y, zs))
  case (runDelayOr False (extEq x y) limit, runDelay limit (extEq x' y')) of
    (True, Now True ) -> pure ()
    (True, Now False) -> fail (show x' ++ " /= " ++ show y')
    _                 -> discard
```

```{.unwrap pipe="NAME=extEqCannotBeDistinguished ./run"}
main = defaultMain extEqCannotBeDistinguished
```

However, this doesn't give us much confidence since we can only prove inequality
in certain specific cases. Expressions which are unequal in a way we can't prove
will give a never-ending sequence of `Later`{.haskell}, and hence be discarded.
There *is* a way we can assert that all the results are extensionally equal, by
removing the timeout: that's risky, since if we're wrong then one of those
never-ending sequences will cause our test suite to run forever!

```{.haskell pipe="./show"}
-- | Runs a 'Delay' value to completion. This may run forever!
unsafeRunDelay :: Delay a -> a
unsafeRunDelay (Now   x) = x
unsafeRunDelay (Later x) = unsafeRunDelay x

extEqAreInterchangable = testProperty "extEqAreInterchangable" $ do
  -- For safety, we can only use the smart generator
  (x, y, zs) <- gen $ genSwappableExtEqValsN limit
  let (x', y') = (unzipCom (x, zs), unzipCom (y, zs))
  case (unsafeRunDelay (extEq x y), unsafeRunDelay (extEq x' y')) of
    (True, True ) -> pure ()
    (True, False) -> fail (show x' ++ " /= " ++ show y')
    _             -> discard
```

```{pipe="cat > timed && chmod +x timed"}
#!/bin/sh
set -eu
set -o pipefail
GIVEN=$(cat)
for RETRY in $(seq 1 10)
do
  if GOT=$(echo "$GIVEN" | withTimeout ./run) 2> er
  then
    cat er 1>&2
    echo "$GOT"
    exit 0
  else
    echo "Retrying $NAME $RETRY" 1>&2
  fi
done
cat er 1>&2
exit 1
```

```{.unwrap pipe="NAME=extEqAreInterchangable MAX_SECS=120 ./timed"}
main = defaultMain extEqAreInterchangable
```

(If you look at
[this page's Markdown
source](/git/chriswarbo-net/git/branches/master/unfinished/falsifying_myself.html)
you'll see that I'm actually a coward, since I'm wrapping the above Haskell
process in a timeout of a few minutes, just in case!)

### Extensionally unequal values are distinguishable ###

When two values are extensional *unequal*, there exist input values for which
they disagree. Note that we can't test this by just generating some input values
and asserting that they disagree, since *some* inputs may happen to agree by
coincidence. Instead, we'll generate a `Stream`{.haskell} of input
`Stream`{.haskell}s, and check for disagreement on all of them, and interleave
their execution using `race`{.haskell}:

```{.haskell pipe="./show"}
-- | Generate a pair of 'Com' values which are extensionally unequal
genExtUneqN :: Fuel -> Gen (Com, Com)
genExtUneqN = genMatching genComN uneq shrinkCom
  where uneq n x y = runDelayOr False (not . isJust <$> extensionalInputs x y) n

genExtUneq = genExtUneqN limit

extUneqWillDisagree = testProperty "extUneqWillDisagree" $ do
    (x, y) <- gen genExtUneq
    len    <- gen genFuel
    inputs <- gen genInputs
    let result = race (sAt len . agree x y <$> inputs) >>= findDisagreement
    pure (unsafeRunDelay result)
  where genInputs = Cons <$> genComs <*> genInputs
        findDisagreement (x, _, xs) = if diff x
                                         then pure ()
                                         else race xs >>= findDisagreement
```

```{.unwrap pipe="NAME=extUneqWillDisagree MAX_SECS=120 ./timed"}
main = defaultMain extUneqWillDisagree
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
