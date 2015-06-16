---
title: Resource-Bounded Complexity
---

```{pipe="cat > addCode"}
#!/bin/sh
tee -a code.hs
echo -e "\n" >> code.hs
```

```{pipe="cat > addProperty"}
#!/bin/sh
prop=$(cat)
func="\$@ -> $prop"
echo -e "quickCheck ($prop)\n" >> tests.hs
```

`chmod +x add*`{pipe="sh > /dev/null"}

I've written a lot about search procedures before: programs which generate values of some 'universal' format which satisfy some predicate. By 'universal' I mean a type which is unbounded, easy to construct values of and easy to encode/decode other types into/from. In this article I'll use `type BitString = List Boolean`{.haskell pipe="./addCode"}, since it's simple and isn't a million miles away from a modern computer's RAM.

For example, if we have some `type Predicate = BitString -> Boolean`{.haskell pipe="./addCode"}, we would like to define a function `search :: Predicate -> BitString`{.haskell pipe="./addCode"}, which satisfies the property that `p (search p) == True`{.haskell pipe="./addProperty"}. We can represent this with the *dependent* type `search :: (p :: Predicate) -> (b :: BitString, p b == True)`{.haskell}.

All search procedures are subject to the [*No Free Lunch Theorem*](http://en.wikipedia.org/wiki/No_free_lunch_theorem), which prevents them being completely general (the ability to solve *all* predicates efficiently). To be efficient for some predicates, we must be inefficient for others. Thankfully, we care more about some predicates than others. In particular, we care about *simple* predicates more than *complex* ones; since they're more likely to occur in the real world.

Of course, this depends on how we define complexity.

## Kolmogorov Complexity ##

[Kolmogorov complexity](http://en.wikipedia.org/wiki/Kolmogorov_complexity) requires us to choose some programming language. Since `BitString`{.haskell} is a universal format, we can say `type Program = BitString`{.haskell pipe="./addCode"}. We just need some way of running `Program`{.haskell}s, which I'll call `eval :: Program -> Solution -> Bool`{.haskell pipe="./addCode"}.

Consider all `p :: Program`{.haskell} such that `eval p = x`{.haskell}, for some value of `x :: BitString`. We define the Kolmogorov complexity of `x`{.haskell} as the length of the *shortest* program `p`{.haskell}.

Finding the Kolmogorov complexity of a value is, in general, undecidable. The only way we can attempt to calculate it is via brute force:

```{.haskell pipe="./addCode"}
allBitStrings = let f xs = xs ++ f (map (0:) xs ++ map (1:) xs)
                 in [] ++ f [0, 1]

k x = filter ((== x) . eval) allBitStrings
```

Of course, this will hit an infinite loop as soon as `eval`{.haskell} is applied to a non-halting program. We might try to fix this by only checking those programs which halt and ignoring those which don't. However, that can't be done due to the undecidability of the halting problem.

## Naive Complexity ##

One way to avoid the halting problem is to weaken our the problem and only check programs which *provably* halt; skipping those which don't *provably* halt. The halting problem tells us that the latter category will always include programs which *do* halt, but which we can't *prove*; ie. it allows *false negatives* (but not false positives). This is known as a *conservative* approach, and proving whether a program halts is called *termination checking*.

Due to the presence of false negatives, such a procedure would not be calculating the Kolmogorov complexity; it would give us some weaker variant.

We can define a simple, conservative, decidable termination checker by checking whether a program halts *within `t`{.haskell} time steps*, for some fixed value `t`{.haskell}. We can augment our `eval`{.haskell} function to get `evalFor :: Nat -> BitString -> Maybe BitString`{.haskell}, such that `evalFor t p = Just x`{.haskell} if `p`{.haskell} halts with the return value `x`{.haskell} in fewer than `t`{.haskell} steps, or `eval t p = Nothing`{.haskell} otherwise.

The larger the value of `t`{.haskell}, the fewer false negatives our termination checker will produce, but the longer it will take to run.

# Parameterless Complexity ##

Having to choose a value for `t`{.haskell} is pretty hacky. One straightforward way to get rid of this parameter is to try all possible values, starting at `1`{.haskell}, one after another. The only problem is that there are infinitely many programs, so we'll never finish testing all of them for `t = 1`{.haskell}, and hence we'll never move on to `t = 2`{.haskell}!

We can get around this by limiting the *length* of our programs: there are only finitely many programs of length `n`{.haskell}; in our `BitString`{.haskell} example there are `2^n`{.haskell}. Hence we can check all programs of length `n`{.haskell} for `t = 1`{.haskell} in finite time, followed by `t = 2`{.haskell} in finite time, and so on. Of course, we can also check all programs of length `n = 1`{.haskell} for time `t`{.haskell}, then all programs of length `n = 2`{.haskell} for time `t`{.haskell}, and so on.

Since the rate of false negatives decreases monotonically as `n`{.haskell} *or* `t`{.haskell} are increased, we can combine them into one value `l = t = i`{.haskell}. We then have a straightforward algorithm which checks all programs up to length `i`{.haskell} for up to `i`{.haskell} steps, then all programs up to length `i + 1`{.haskell} for up to `i + 1`{.haskell} steps, and so on. Let's call this `naiveSearch :: BitString -> BitString`{.haskell}.

Let's say we have some program `P` has length `lP`{.haskell} and halts in time `tP`{.haskell}, we can ask the question: how long will it take `naiveSearch P`{.haskell}

*Levin complexity* selects all programs up to some length `l`{.haskell} and sends them through this conservative termination checker to see if they halt We can eliminate parameters like `t` by running the system again and again in 'phases'; doubling the value of `t`{.haskell} each time. This technique is used, for example, by parameterless genetic algorithms.
