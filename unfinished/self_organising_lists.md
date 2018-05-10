---
title: Self-Organising Lists
packages: [ 'ghc' ]
---

<!-- Helper scripts -->

```{pipe="cat > hs"}
tee -a code.hs
echo "" >> code.hs
```

```{pipe="cat > ghci"}
CODE=$(cat)
PREFIX=":load code.hs"
printf "${PREFIX}\n${CODE}" | ghci -v0
```

`chmod +x hs ghci`{pipe="sh > /dev/null"}

<!-- Content -->

I was marking a Haskell assignment recently which introduced the concept of
[self-organising lists](http://en.wikipedia.org/wiki/Self-organizing_list)
(specifically the "move to front" variety).

## The Context ##

Let's say we have a tail-recursive list type. Haskell's Prelude defines one,
called `forall a. [a]`{.haskell}. It's conventional to leave the
`forall a.`{.haskell} implicit, so I'll just write `[a]`{.haskell} to mean the
list type.

We can use `[]`{.haskell} to construct an empty list and `:`{.haskell} to
prepend an element to another list.

We want the ability to look up an element which matches some predicate, eg. "has
the ID '100'", or "is shorter than 10", or whatever. I'll represent predicates
using the function type `a -> Bool`{.haskell}.

Implementing such a lookup function is pretty straightforward:

```{.haskell pipe="./hs"}
lookUp :: (a -> Bool) -> [a] -> Maybe a
lookUp p []     = Nothing
lookUp p (x:xs) = if p x
                     then Just x
                     else lookUp p xs
```

In the assignment we used `a`{.haskell} as the return type and allowed errors to
be thrown if no match was found, but that's a horrible way to do it. Real
Haskell code would avoid the errors completely by using the
[`Maybe`{.haskell}](https://wiki.haskell.org/Maybe) type.

We can verify that this works using a few examples:

```{pipe="cat > lookUps"}
print (lookUp (== 5) [1, 2, 3, 4, 5, 6, 7])
print (lookUp (== 5) [10, 11, 12])
```

```{.haskell pipe="sh"}
cat lookUps
```

```{.haskell pipe="sh"}
./ghci < lookUps
```

## The Problem ##

The `lookUp`{.haskell} function steps through the list one element at a time
until it finds a match. We can calculate its computational complexity by
counting how many times it runs the predicate function `p`{.haskell}, in terms
of the length $N$ of the list. This should be roughly proportional to the amount
of time it takes.

In the best case, the first element of the list satisfies the predicate, so we
only run the predicate once. This gives best-case complexity of $O(1)$.

In the worst case, either *no* element satisfies the predicate, or only the
*last* element of the list satisfies the predicate. In both of these cases, we
must run the predicate on each element of the list, giving worst-case complexity
of $O(N)$. Since the behaviour in both of these cases is identical, I'll assume
there's always a match from now on.

In general, the complexity depends on the position of the first element which
satisfies the predicate. Hence the *average* (or *expected*) complexity depends
on the *average* position. With no prior knowledge of how our function will be
used, the average position is $\frac{N}{2}$, hence the expected complexity is
$O(\frac{N}{2})$.

If we knew more about how our function was going to be used, we could improve
its performance. For example, let's say that `lookUp`{.haskell} will only be run
with the constant list `myList`{.haskell}. Let's also say that one particular
predicate is much more likely to be used than any other; let's call it
`p1`{.haskell}.

The first thing we can do is make a `lookUp`{.haskell} function which hard-codes
the list `myList`{.haskell}:

```{.haskell}
lookUpMyList p = lookUp p myList
```

Any speedup this gives us will be modest (e.g. due to compiler optimisations,
like loop unrolling). We can do much better by looking up a match (if any) for
`p1`{.haskell} ahead of time, and putting it at the start of the list:

```haskell
-- 'acc' accumulates elements from the given list (in reverse order)
optimiseList :: [a] -> (a -> Bool) -> [a] -> Maybe [a]
optimiseList acc p []     = reverse acc  -- No match, list will be unchanged
optimiseList acc p (x:xs) = if p x
                               then Just (x : reverse acc ++ xs)  -- Match found
                               else optimiseList (x:acc) p xs
```

Now we can look up matches from an optimised version of `myList`{.haskell}:

```haskell
lookUpOptimised = let newList = optimiseList [] p1 myList
                      go p    = lookUp p newList
                   in go
```

The first time `lookUpOptimised`{.haskell} is called, the local definition
`go`{.haskell} will be evaluated, resulting in a function. When that function
is called, `lookUp`{.haskell} will be called, which will force
`newList`{.haskell} to be evaluated. The `lookUp`{.haskell} function will
perform as usual, but in those cases where it's called with `p1`{.haskell}
(which we're assuming is quite often), and when `myList`{.haskell} does actually
contain an element satisfying `p1`{.haskell}, then `lookUp`{.haskell} will hit
that element immediately since it will be at the start of `newList`{.haskell},
and hence this (assumed to be common) use-case has the $O(1)$ best-case
behaviour. Note that we'll still get the $O(n)$ worst-case behaviour if there is
no element in `myList`{.haskell} which satisfies `p1`{.haskell}, and we'll also
pay a one-time cost of `$O(n)$ to do the initial search either way.

This is all well and good, but its assumptions are quite strong. What if we
*don't* know what predicates will be common, or what if there are several common
predicates? This is where self-organising lists come in.

## Self-Organising List Lookup ##

To make a self-organising list we just need to combine the processing performed
by `lookUp`{.haskell} and `optimiseList`{.haskell}. When we're asked to look
up a value in a list, rather than *just* returning a matching element (if found)
we *also* return a permutation of the list optimised for that predicate:

```haskell
selfOrganisingLookUp :: (a -> Bool) -> [a] -> ([a], Maybe a)
selfOrganisingLookUp =
  let go acc p [] = (reverse acc, Nothing)
      go acc p (x:xs) = if p x
                           then (x : reverse acc ++ xs, Just x)
                           else go (x:acc) p xs
```

To see any benefit we need to ensure that the list returned from each lookup is
used as input to the following lookup. We can do this by propagating the
resulting list up through our return values, all the way to our "main loop",
then pass them into the next iteration. We can do this explicitly, or get fancy
with monads, applicatives, etc.

## Analysing Performance ##

The complexity of `selfOrganisingLookUp`{.haskell} is tricky, and depends on the
ratio of common-element lookups to non-common-element lookups. The best and
worst case complexity match the regular list, $O(1)$ and $O(N)$, but the
rearranging increases the chance that we'll hit the best case, if some queries
are more frequent than others. Hence, the self-organising list is *at least as
fast* as the regular list, when lookups are drawn at random from some fixed
distribution.

It's also interesting to consider an "adversarial" situation, where an attacker
knows that we're using a self-organising list and wants to take down some
service that we're providing.

By choosing queries which match the *last* element of the list, an attacker can
make this system work harder than then a normal list would for the same
queries. However, a similar attack is possible by providing queries with no
matches, since they'll always check the whole list; or by timing how long it
takes to perform certain queries and performing the slow ones over and over
(which will never get a speed up when using regular lists).

### A Poorly Performing Implementation ###

One student submitted an implementation something like the following:

```haskell
selfOrganisingLookUp p xs = let notP x = not (p x)
                                ys     = filter    p xs
                                zs     = filter notP xs
                             in (ys ++ zs, listToMaybe ys)
```

This approach produce *almost* the right output: it moves *all* matching
elements to the front of the list; since only one is needed to speed up repeated
queries, there's nothing to be gained by moving multiple matches to the front;
in fact this will *slow down* repetitions of *prior* queries, since they may
have to skip past all of these front elements before they reach the match we
previously prepended for them.

Despite the mostly-correct input/output behaviour, this solution is actually
pretty bad due to the *way* it's implemented. By using `filter`{.haskell} and
`++`{.haskell} in this way, the input list will often get fully traversed
*twice*: once to look for elements which match `p`{.haskell}, once for elements
which don't. It's slightly complicated by lazy evaluation, but it's generally
pretty bad since we're duplicating work.

The moral is that there's more to software than just correct input/output
behaviour: there are many other 'non-semantic' features of an implementation.
Since the only point of using self-organising lists is to speed up queries
compared to using regular lists, we're actually *doubly* worse off with this
implementation: it's *slower*, which is the opposite of what we wanted; but it's
also *more complicated* than a function like `lookUp`{.haskell}. I'm a big fan
of simple code, even if it's slow; in this case it's more complicated and
slower. If I came across this in the wild I would just replace it with the
built-in list functions. In the rare event that this became a bottleneck,
demonstrated by profiling data, then I would set up some benchmarks (e.g. using
Criterion and ASV); I'd pull this old code out of git and compare its
performance; I'd optimise both versions (e.g. coming up with my previous, fast
solution); and keep the normal list version around for testing purposes (e.g.
property checking with QuickCheck).

<!-- Sanity checks to abort rendering on error -->

```{pipe="sh > /dev/null"}
echo 'main = print "DONE"' | ./hs
ghc code.hs
```
