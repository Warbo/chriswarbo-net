---
title: Lazy Test Generators
---

I recently learned about [the falsify package for property-based
testing](https://well-typed.com/blog/2023/04/falsify/), which seems like an
improvement over many previous libraries, in the following ways:

 - No need for separate shrinking functions (unlike
   [QuickCheck](https://hackage.haskell.org/package/QuickCheck))
 - Generating and shrinking works reliably across monadic calls
   (unlike [Hedgehog](https://hedgehog.qa))
 - Shrinking is simple, predictable and monotonic (unlike
   [Hypothesis](https://hypothesis.works))

The only feature it seems to lack is allowing parts of counterexamples to remain
undefined, as found in the
[LazySmallCheck2012](https://github.com/UoYCS-plasma/LazySmallCheck2012)
package. I've opened [a feature request in falsify for
this](https://github.com/well-typed/falsify/issues/70), and the author has
replied with a rather simple approach that I'm looking forward to experimenting
with. The rest of this post is mostly copy-pasta from that request, since it
serves as a nice resource for describing the idea of lazy counterexamples.

As a bonus, in order to produce example output for that feature request, I ended
up fixing
[my fork of LazySmallCheck2012](https://github.com/warbo/LazySmallCheck2012) to
work with GHC 9+ (and in particular the breaking changes made in Template
Haskell 2.18). I've also given it a simple `default.nix` definition, and a
development environment in `shell.nix`, so it should be easier to pick up
(rather than trial-and-error with Cabal version constraints).

Note that I don't recommend *using* LazySmallCheck2012 for your own projects,
since it's mostly abandonware. I've actually been migrating my own projects away
from it to avoid having to maintain that fork: I *was* migrating them to
QuickCheck (since I've been using that for over a decade anyway), but going
forward I'll certainly be using falsify. Nevertheless, I think
LazySmallCheck2012 is great as a sort of tech-demo to play around with!

### What is lazy generation ###

LazySmallcheck2012 behaves in a way where data is only generated when the test
forces its evaluation. Note that the original lazysmallcheck package *does not*
behave this way (at least, based on my experiments).

For example, consider this simple test in falsify:

```haskell
prop_unneededElements :: Property ()
prop_unneededElements = do
  lst :: [Int] <- gen (Gen.list
                        (Range.between (0, 100))
                        (Gen.inRange (Range.between (0, 100))))
  if length lst > 3 && head lst < 10
     then testFailed "known failure, to check strictness"
     else return ()
```

This is asserting that every `lst :: [Int]`{.haskell} with more than three
elements also has a first element that is at least `10`{.haskell}. It fails (as
expected):

```haskell
Running 1 test suites...
Test suite falsify-test-test: RUNNING...
Playing
  Unneeded elements: FAIL (0.12s)
    failed after 28 successful tests and 5 shrinks
    known failure, to check strictness
    Logs for failed test run:
    generated [0,0,0,0] at CallStack (from HasCallStack):
      gen, called at test/Main.hs:18:19 in main:Main

    Use --falsify-replay=010985dabce2ff4eb5c78d1e70747b669b to replay.

1 out of 1 tests failed (0.13s)

Test suite falsify-test-test: FAIL
```

In this case falsify reports a minimal counterexample of
`[0, 0, 0, 0]`{.haskell}. Whilst that's the smallest fully-normalised value, it
is actually over-specified: the test never forces the last three elements, so
they are irrelevant to the outcome (only the spine and the first element are
relevant). If we compare this to the same check in LazySmallcheck2012 (in GHCi):

```haskell
*LSC2012> test (\(lst :: [Int]) -> not (length lst > 3 && head lst < 10))
LSC: Depth 0:
LSC: Property holds after 3 tests.
LSC: Depth 1:
LSC: Property holds after 5 tests.
LSC: Depth 2:
LSC: Property holds after 7 tests.
LSC: Depth 3:
LSC: Property holds after 9 tests.
LSC: Depth 4:
LSC: Counterexample found after 12 tests.
Var 0: -3:_:_:_:[]
*** Exception: ExitFailure 1
```

It gives `-3:_:_:_:[]`{.haskell}, AKA `[-3, _, _, _]`{.haskell}. Here `_`
indicates a value which was never forced, so never ran its generator, and is
hence irrelevant to the cause of the test failure. Unfortunately in this case
the first value (`-3`{.haskell}) isn't minimal, since (Lazy)Smallcheck doesn't
do any shrinking; the ideal counterexample would be `[0, _, _, _]`{.haskell}.

### Why lazy generation ###

There are a few reasons this behaviour is nicer, as a user, than seeing fully
normalised counterexamples:

 - Minimal values, like `0`{.haskell}, are ambiguous: sometimes it means they
   can be ignored, but sometimes there is significance to that value. Using
   `_`{.haskell} for the former removes some ambiguity.
 - If we expect a value to be important for branching, seeing it appear as
   `_`{.haskell} indicates a problem with our control flow.
 - Using `_`{.haskell} instead of a particular value avoids accidental
   coincidences that we can waste time ruling out, e.g. `[0,0,0,0]`{.haskell}
   contains duplicate elements, but that's just a coincidence and isn't related
   to the failure.
 - More properties can be *proved* correct. Those with small, finite domains
   like `Bool`{.haskell} can already be proved by exhaustion; lazy generation
   complements this, since a property which passes without forcing some part of
   its input must therefore be true *for all* values of that part.

There *may* be performance benefits, by avoiding a bunch of generators running;
but I haven't found that particularly noticable.

### How LazySmallcheck2012 does it ###

As far as I know, LazySmallcheck2012 works by starting with all inputs as
exceptions, and retrying a test with more refined inputs if any of those
exceptions are thrown (until some depth limit is reached). In the above example
it might have run the property on an input like `throw A`{.haskell}, which
causes the exception `A`{.haskell} to be thrown; so it ran again with
`[]`{.haskell} (which passed) and `throw A : throw B`{.haskell} (which throws
`B`{.haskell}). The latter is refined to `throw A : []`{.haskell} (which passed)
and `throw A : throw B : throw C`{.haskell} (which throws `C`{.haskell}) and so
on until `throw A : throw B : throw C : throw D : []`{.haskell} finally
satisfies the `length`{.haskell} check, and causes the `head`{.haskell} check to
throw `A`{.haskell}; causing it to be retried with an input like
`-3 : throw A : throw B : throw C : []`{.haskell} which finally causes the test
to fail, and that is our counterexample (with `throw X`{.haskell} shown as
`_`{.haskell}).

### How to do this in falsify ###

falsify's author [commented on how this could be
achieved](https://github.com/well-typed/falsify/issues/70#issuecomment-1898087718)
(in a much simpler and more elegant way that my suggestion!), by essentially
wrapping values of `a`{.haskell} into `Either () a`{.haskell}, turning a
`Left ()`{.haskell} into `undefined :: a`{.haskell}, shrinking towards the
`Left`{.haskell} and `Show`{.haskell}ing it as `"_"`{.haskell}.

That seems like a good approach, although I'm going to try a couple of further
improvements:

 - Firstly by making this change "downstream" of the generator output, so it
   applies automatically to all *parts* of a generated value, without having to
   manually unwrap at every step.
 - Secondly, replacing the use of `undefined`{.haskell} with some
   uniquely-identifiable mechanism (e.g. throwing a private exception). That way
   we can distinguish between "code under test caused `undefined`{.haskell}"
   versus "our lazy generation shenanigans got forced". This is important, since
   the former is a test failure, and hence a valid counterexample; whilst the
   latter is an artefact of shrinking and should be discarded (as if the test
   passed)
