---
title: Exploring Bag/Box/Multiset Arithmetic
---

N J Wildberger has been publishing [a fascinating series of
videos](https://www.youtube.com/watch?v=4xoF2SRp194&list=PL5A714C94D40392AB&index=262)
about [multisets](https://en.wikipedia.org/wiki/Multiset), and how they can be
used to implement arithmetic: not only for integers, but also polynomials and
multinomials. Wildberger also calls these "boxes", but as a programmer I'll
stick to the more conventional term
["bags"](https://www.nist.gov/dads/HTML/bag.html)!

<details class="odd">
 <summary>Note</summary>

The linked video is titled "part 2", and is more than 200 videos deep into
Wildberger's "Math Foundations" playlist; nevertheless, it is the start of a
contiguous series on multiset arithmetic. The "part 1" seems to be [Multisets
and a new framework for
arithmetic](https://www.youtube.com/watch?v=vZ5ItJkfLy4&list=PL5A714C94D40392AB&index=190),
which was made a several years earlier and has subtle differences in approach
(e.g. using multisets of "marks", like a tally system; whilst the later series
simplifies to be multisets of zero)

</details>

In this post I want to apply techniques from *theory exploration* to this theory
of bags. This is where a computer program is given the relevant definitions, and
it uses various search techniques to automatically discover patterns and
relationships between those definitions (i.e. conjectures and theorems).

I'll be writing code in Haskell, since that has many good theory exploration
tools (some of which I wrote ;) ).

## Representing Bags ##

There are many ways to represent bags, which are mostly isomorphic (although
they may differ in non-functional aspects; e.g. a
[conc-list](https://en.wikipedia.org/wiki/Conc-tree_list) is better for parallel
processing than a [cons-list](https://en.wikipedia.org/wiki/Cons#Lists)). I'll
stick to cons-lists, since they're the most familiar. The most important
decision is whether to stratify our bags, i.e. should "bags of bags" have the
same type as a "bag of bags of bags"?

Here's an unstratified implementation, called `UBag`: every element of a `UBag`
is another `UBag`, with arbitrary nesting allowed.

```haskell
data UBag = UEmpty
          | UInsert UBag UBag
```

Every `UBag` is either `UEmpty` (an empty `UBag`), or `UInsert foo bar`. The
latter is ambiguous about which is being inserted and which we're inserting
into: we'll declare that the first `UBag` (`foo`) is being inserted into the
second (`bar`).

Here are some of Wildberger's definitions, implemented with `UBag`:

```haskell
-- Zero
zero = UEmpty

-- Nat
one = UInsert zero UEmpty
two = UInsert zero one

-- Poly
alpha = UInsert one UEmpty
```

represents inserting the `UBag` `foo` into the `UBag` bar.
At the moment this is identical to the usual definition of *singly-linked
lists*: to get th behaviour of a `Bag` we need to discard the information about
what order the elements have been "put in". We can do this by avoiding direct
use of the `PutIn` constructor, and instead using a function which sorts the
resulting `Bag` into some canonical ordering; and hence discarding the
irrelevant information about which order items were put in to the bag. (Note:
such functions are informally known as "smart constructors")

```haskell
--| Smart constructor for putting a bag in a bag. Ensures that the order of
--  insertion doesn't matter.
putIn :: Bag -> Bag -> Bag
putIn x y = sortBag (PutIn x y)

--| Sort a Bag into a canonical order, discarding information about the order in
--  which its contents were inserted.
sortBag :: Bag -> Bag
sortBag Empty = Empty
sortBag (PutIn x y) =
    let x' = sortBag x in
      case sortBag y of
        Empty                 => PutIn x' Empty
        PutIn y z | x' <<= y   => PutIn x' (PutIn y z)
        PutIn y z | otherwise => PutIn y (putIn x' z)

sortedLessThanEqual :: Bag -> Bag -> Bag
sortedLessThanEqual x y = case (x, y) of
  (Empty     , Empty     ) -> True
  (Empty     , _         ) -> True
  (_         , Empty     ) -> False
  (PutIn x xs, PutIn y ys) ->


--| Implement a total order on Bag, so we can compare them
instance Ord Bag where
  compare Empty        Empty        = EQ
  compare Empty        (PutIn _ _)  = LT
  compare (PutIn _ _)  Empty        = GT
  compare (PutIn x xs) (PutIn y ys) = case putIn sortBag xs

--| Ord requires we also implement equality
instance Eq Bag where
  Empty     == Empty     = True
  Empty     == PutIn _ _ = False
  PutIn _ _ == Empty     = False

```

`Eq`ality and `Ord`ering this is because Haskell expressions are written in some order, have we need to Next we tell Haskell how

isWe only care about
Whilst bags, sets,
lists, etc. tend to be generic/polymor
