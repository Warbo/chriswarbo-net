---
title: Resource-Bounded Complexity
---
I've written a lot about search procedures before: programs which generate values of some 'universal' format which satisfy some predicate. By 'universal' I mean a type which is unbounded, easy to construct values of and easy to encode/decode other types into/from. In this article I'll use `type BitString = List Boolean`{.haskell}, since it's simple and isn't a million miles away from a modern computer's RAM.

For example, if we have some type `Predicate = BitString -> Boolean`{.haskell}, we would like to define a function `search :: Predicate -> BitString`{.haskell}, which satisfies the property that `p (search p) == True`{.haskell}. We can represent this with the *dependent* type `search :: (p :: Predicate) -> (b :: BitString, p b == True)`{.haskell}.

All search procedures are subject to the [*No Free Lunch Theorem*](http://en.wikipedia.org/wiki/No_free_lunch_theorem), which prevents them being completely general (the ability to solve *all* predicates efficiently). To be efficient for some predicates, we must be inefficient for others. Thankfully, we care more about some predicates than others. In particular, we care about *simple* predicates more than *complex* ones; since they're more likely to occur in the real world.

Of course, this depends on how we define complexity.

## Kolmogorov Complexity ##

[Kolmogorov complexity](http://en.wikipedia.org/wiki/Kolmogorov_complexity) requires us to choose some programming language. Since `BitString`{.haskell} is a universal format, we can use it to represent programs too: `Program = BitString`{.haskell}, so we just need some way of running `Program`{.haskell}s, which I'll call `eval :: Program -> BitString`{.haskell}.

Consider all `p :: Program`{.haskell} such that `eval p = x`{.haskell}, for some value of `x :: BitString`. We define the Kolmogorov complexity of `x`{.haskell} as the length of the *shortest* program `p`{.haskell}.

Finding the Kolmogorov complexity of a value is, in general, undecidable. The only way we can attempt to calculate it is via brute force:

```haskell
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

We can define a naive complexity measure by considering the *length* of a program

*Levin complexity* selects all programs up to some length `l`{.haskell} and sends them through this conservative termination checker to see if they halt We can eliminate parameters like `t` by running the system again and again in 'phases'; doubling the value of `t`{.haskell} each time. This technique is used, for example, by parameterless genetic algorithms.
