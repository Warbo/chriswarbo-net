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
optimiseList :: [a] -> (a -> Bool) -> [a] -> [a]
optimiseList acc p []     = reverse acc                        -- No match
optimiseList acc p (x:xs) = if p x
                               then x : reverse acc ++ xs      -- x matches
                               else optimiseList (x:acc) p xs  -- Recurse on xs
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
`newList`{.haskell} to be evaluated. Since `newList`{.haskell} is defined
*outside* the `go`{.haskell} function, it will be reused in subsequent calls to
`lookUpOptimised`{.haskell}, rather than being calculated again (in fact, we
would probably see the same performance even if we wrote `newList`{.haskell}
inside `go`{.haskell}, since Haskell implementations may spot that it's a
constant and "float" it outside). The `lookUp`{.haskell} function will perform
as usual, but in those cases where it's called with `p1`{.haskell} (which we're
assuming is quite often), and when `myList`{.haskell} does actually contain an
element satisfying `p1`{.haskell}, then `lookUp`{.haskell} will hit that element
immediately since it will be at the start of `newList`{.haskell}, and hence this
(assumed to be common) use-case has the $O(1)$ best-case behaviour. Note that
we'll still get the $O(n)$ worst-case behaviour if there is no element in
`myList`{.haskell} which satisfies `p1`{.haskell}, and we'll also pay a one-time
cost to do the initial search either way ($O(n)$ in the worst case).

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
  let go acc p [] = (reverse acc, Nothing)                      -- No match
      go acc p (x:xs) = if p x
                           then (x : reverse acc ++ xs, Just x) -- x matches
                           else go (x:acc) p xs                 -- Recurse on xs
   in go []  -- Start with an empty accumulator
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
fast* (asymptotically) as the regular list, when lookups are drawn at random
from some fixed distribution. In reality there is some constant overhead caused
by propagating the new lists around, and an $O(N)$ overhead from calling
`reverse`{.haskell} (although we're only reversing the part of the list that we
had to check, so this will match the complexity of the lookup: $O(1)$ when a
match was found near the start, $O(N)$ when it was near the end or not found).

It's also interesting to consider an "adversarial" situation, where an attacker
knows that we're using a self-organising list and wants to take down some
service that we're providing.

By sending lots of queries, an attacker can manipulate the overall order of the
list, and hence come up with queries whose matches are near the end, causing us
to hit the $O(N)$ worst-case performance again and again, which may be enough to
slow down or disable our server. However, this isn't too much of a worry since
a simpler attack can cause the same behaviour by repeatedly sending a query
which has no matches: that would also trigger the $O(N)$ behaviour, but doesn't
require manipulation of the order. Since this simpler attack works on regular
lists too, I don't see much disadvantage to using self-organising lists. Even if
we limited ourselves to queries which always match, regular lists can be subject
to a timing attack: timing how long different queries take, then sending the
slowest ones again and again. Self-organising lists aren't vulnerable to such
repetitions of the same query, so at least they'd force attackers to work a
little harder.

### A Poorly Performing Implementation ###

For the assignment, one student submitted an implementation something like the
following:

```haskell
selfOrganisingLookUp p xs = let notP x = not (p x)
                                ys     = filter    p xs
                                zs     = filter notP xs
                             in (ys ++ zs, listToMaybe ys)
```

This approach produces *almost* the right output, so they wanted almost full
marks. However, there are three problems with this implementation.

Firstly, the behaviour is not *quite* right: it moves *all* matching elements to
the front of the list, but since only one is needed to speed up repeated
queries, there's nothing to be gained by looking for any others; we're hence
performing extra work (looking for more matches) which is of no benefit, slowing
us down. This can actually make the implementation *slower* than regular lists,
when averaged over multiple calls (laziness saves us from doing all of this
extra work on the first call, but only manages to defer it to subsequent calls).

The second problem is that moving multiple matches to the front will *slow down*
repetitions of *prior* queries, since they may have to skip past all of these
front elements before they reach the match we previously prepended for them. For
example, say our list is `[1, 2, 3, ..., 200]`{.haskell} and the most common
queries are for even numbers and odd numbers. Both versions would return the
even number `2`{.haskell}, but the fast version would return the list
`[2, 1, 3, 4, 5, ..., 200]`{.haskell} whilst the student's version would
return `[2, 4, 6, ..., 200, 1, 3, 5, ..., 199]`{.haskell}. Querying the fast
result for an odd number would quickly find `1`{.haskell}, resulting in the list
`[1, 2, 3, ..., 200]`{.haskell}; yet the student's version would have to check
half of the list before it found any odd number, rearranging the result to be
`[1, 3, 5, ..., 199, 2, 4, 6, ..., 200]`{.haskell}. In other words, moving
multiple matches to the front will disrupt the optimisations performed by
previous queries, for no benefit and significant extra cost.

If the assignment were just about spitting out particular values then these
issues wouldn't be too bad; but since the context is that self-organising lists
are an *optimisation* compared to regular lists, this implementation *isn't* a
very good solution since it's actually *slower*.

The third problem isn't specific to this assignment, and it isn't related to the
input/output behaviour at all: this implementation is more *complicated* than a
function like `lookUp`{.haskell}. I'm a big fan of simplifying code, even at the
expense of speed (if speed's an issue, profile it and benchmark). Yet this code
is not only more complicated than it needs to be, it's more complicated than the
thing it's replacing. If I came across this in the wild I wouldn't hesitate to
rip it out and replace it with built-in list functions, since they're so much
simpler.

<!-- Sanity checks to abort rendering on error -->

```{pipe="sh > /dev/null"}
echo 'main = print "DONE"' | ./hs
ghc code.hs
```
