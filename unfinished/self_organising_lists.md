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

I was marking a Haskell assignment recently which introduced the concept of [self-organising lists](http://en.wikipedia.org/wiki/Self-organizing_list) (specifically the "move to front" variety).

## The Context ##

Let's say we have a tail-recursive list type. Haskell's Prelude defines one, called `forall a. [a]`{.haskell}. It's conventional to leave the `forall a.`{.haskell} implicit, so I'll just write `[a]`{.haskell} to mean the list type.

We can use `[] :: [a]`{.haskell} to construct an empty list and `(:) :: a -> [a] -> [a]`{.haskell} to prepend an element to another list.

We want the ability to look up an element which matches some predicate, eg. "has the ID '100'", or "is shorter than 10", or whatever. I'll represent predicates using the function type `a -> Bool`{.haskell}.

Implementing such a lookup function is pretty straightforward:

```{.haskell pipe="./hs"}
lookUp :: (a -> Bool) -> [a] -> Maybe a
lookUp p []     = Nothing
lookUp p (x:xs) = if p x
                     then Just x
                     else lookUp p xs
```

In the assignment we used `a`{.haskell} as the return type and allowed errors to be thrown if no match was found, but that's a horrible way to do it. Real Haskell code would avoid the errors completely by using the [`Maybe`{.haskell}](https://wiki.haskell.org/Maybe) type.

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

The `lookUp`{.haskell} function steps through the list one element at a time until it finds a match. We can calculate its computational complexity by counting how many times it runs the predicate function `p`{.haskell}, in terms of the length $N$ of the list. This should be roughly proportional to the amount of time it takes.

In the best case, the first element of the list satisfies the predicate, so we only run the predicate once. This gives best-case complexity of $O(1)$.

In the worst case, either *no* element satisfies the predicate, or only the *last* element of the list satisfies the predicate. In both of these cases, we must run the predicate on each element of the list, giving worst-case complexity of $O(N)$. Since the behaviour in both of these cases is identical, I'll assume there's always a match from now on.

In general, the complexity depends on the position of the first element which satisfies the predicate. Hence the *average* (or *expected*) complexity depends on the *average* position. With no prior knowledge of how our function will be used, the average position is $\frac{N}{2}$, hence the expected complexity is $O(\frac{N}{2})$.

If we knew more about how our function was going to be used, we could improve its performace. For example, let's say that `lookUp`{.haskell} will only be run with the constant list `myList`{.haskell}. Let's also say that one particular predicate is much more likely to be used than any other; let's call it `p1`{.haskell}.

We can use this information to make `lookUp`{.haskell} much faster, by making use of the following function:

```{.haskell pipe="./hs"}
moveToFront :: (a -> Bool) -> [a] -> [a]
moveToFront = let mtf acc p []     = reverse acc
                  mtf acc p (x:xs) = if p x
                                        then x : reverse acc ++ xs
                                        else mtf (x:acc) p xs
               in mtf []
```

```haskell
-- The old definition, with arguments swapped
lookUpSwap []     p = Nothing
lookUpSwap (x:xs) p = if p x
                      then Just x
                      else lookUp' xs p


-- A version of filter which only removes the *first* non-matching element
filterFirst p []     = []
filterFirst p (x:xs) = if p x
                          then x : filterFirst p xs
                          else xs  -- No recursion

-- Specialise lookUp' to be faster for p1 and myList
lookUpP1 = const . lookUp' (case lookUp' p1 myList of
                                 Nothing -> myList
                                 Just x  -> x : filterFirst (not . p1))
```

Notice that `lookUpP1`{.haskell} is a function, but is calculated from other functions, rather than taking arguments explicitly. The first time `lookUpP1`{.haskell} is called, in the following way:

 - Based on the results of `lookUp' p1 myList`{.haskell}, we choose a list:
    - If the result is `Nothing`{.haskell}, we use `myList`{.haskell}
    - If the result is `Just x`{.haskell}, we rearrange the list to

we're much more likely to call `lookUp`{.haskell} with some predicate `p1`{.haskell} is much more likely to be used than any other, we can run `lookUp p1 myList`{.haskell} right move a matching element to the front of the list and get the best-case performance for these calls.

What if we *don't* know what `p1` is, but we know that *some* predicates will be more common than others? This is where self-organising lists come in.

## Self-Organising List Lookup ##

We can represent a self-organising list with a regular Haskell list, but we

lookupKey db k = let isK  d = get1 d == k
                     notK d = not (isK d)
                     x      = filter  isK db
                     xs     = filter notK db
                  in (head x, x ++ xs)

For reference, here's an alternative implementation, which looks
completely different to both of the above:

lookupKey = lookupKey' []

lookupKey' acc []     k = error ("ID " ++ show k ++ " not found")
lookupKey' acc (x:xs) k = if get1 x == k
                             then (x, x : reverse acc ++ xs)
                             else lookupKey' (x:acc) xs k

Notice that it doesn't do any filtering at all: the elements come in via
"x", "xs" and "acc", and the result always contains "x", "xs" and "acc";
nothing is ever discarded, just re-arranged.

---

The question says that "lookupKey" should give us a "self-organising
list" (SOL). The only reason we care about SOLs is that they allow
faster lookups of commonly-requested items than regular lists do.

For example, consider a chain of lookups with regular lists (using
numbers rather than Data, for simplicity):

lookupReg n []     = error "Not found"
lookupReg n (x:xs) = if x == n
                        then x
                        else lookupReg n xs

lookupReg 4 [1, 2, 3, 4, 5]
lookupReg 3 [1, 2, 3, 4, 5]
lookupReg 4 [1, 2, 3, 4, 5]
lookupReg 4 [1, 2, 3, 4, 5]

If we measure speed as the number of comparisons we make (x == y or
x /= y), then our most common use-case, looking up "4", requires
4 + 4 + 4 = 12 comparisons:

4 == 1, 4 == 2, 4 == 3, 4 == 4,
4 == 1, 4 == 2, 4 == 3, 4 == 4,
4 == 1, 4 == 2, 4 == 3, 4 == 4.

Compare this to the self-organising version, which rearranges the
list each time:

lookupSOL 4 [1, 2, 3, 4, 5]
lookupSOL 3 [4, 1, 2, 3, 5]
lookupSOL 4 [3, 4, 1, 2, 5]
lookupSOL 4 [4, 3, 1, 2, 5]

If "lookupSOL" acts like "lookupReg", it should only require
4 + 2 + 1 = 7 comparisons:

4 == 1, 4 == 2, 4 == 3, 4 == 4,
4 == 3, 4 == 4,
4 == 4.

That's how many comparisons are *required*, so let's see how many your
implementation will use:

4 == 1, 4 == 2, 4 == 3, 4 == 4, 4 == 5, (from getMatches)
4 /= 1, 4 /= 2, 4 /= 3, 4 /= 4, 4 /= 5, (from getRest)
4 == 3, 4 == 4, 4 == 1, 4 == 2, 4 == 5, (from getMatches)
4 /= 3, 4 /= 4, 4 /= 1, 4 /= 2, 4 /= 5, (from getRest)
4 == 4, 4 == 3, 4 == 1, 4 == 2, 4 == 5, (from getMatches)
4 /= 4, 4 /= 3, 4 /= 1, 4 /= 2, 4 /= 5. (from getRest)

In total your approach would do 30 comparisons, regardless of where the
element is. That's less than half the speed of a regular list, which we
were trying to avoid because they're too slow! In this case, we've made
our code more complicated *and* made it slower, so we're always better
off using regular lists which are simpler and faster.

Now consider the "alternative" implementation I showed
above, "lookupKey'":

4 == 1, 4 == 2, 4 == 3, 4 == 4,
4 == 3, 4 == 4,
4 == 4.

Its complexity matches the minimum requirement! The code is still more
complicated than using regular lists, but it does give us a speedup, so
it may be useful in some places where speed is a concern.

We can analyse the runtime complexity of these three approaches
too. Let's say "N" is the length of our list, and use the number of
comparisons as the measure of time, like above:

The number of comparisons made by lookupReg depends on how far through
the list our common value is. In the worst case it's at the end,
requiring O(N) comparisons, in the best case it's at the start, giving
O(1) comparisons and the expected position is N/2, giving an expected
performance of O(N/2).

Your implementation will always perform 2N comparisons, so the best,
worst and expected complexity is O(2N).

The complexity of lookupKey' is more tricky, and depends on the ratio of
common-element lookups to non-common-element lookups. The best and worst
case complexity match the regular list, O(1) and O(N), but the
rearranging increases the chance that we'll hit the best case. Hence,
the self-organising list is *at least as fast* as the regular list, when
lookups are "independent and identically distributed" (IID).

It's also interesting to consider an "adversarial" situation, where an
attacker knows that you're using a self-organising list and wants to
take down some service that you're providing.

By requesting the last element of the list, an attacker can make your
system work harder than normal, which makes DDOS attacks much easier. By
sending lots of requests, they can manipulate the order of elements in
your list, and hence they can predict what the last element will be.

This breaks the IID assumption, and makes such a system perform *worse*
than a regular list!

<!-- Sanity checks to abort rendering on error -->

```{pipe="sh > /dev/null"}
echo 'main = print "DONE"' | ./hs
ghc code.hs
```
