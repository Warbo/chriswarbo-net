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

```{pipe="cat > hide && chmod +x hide"}
{
  echo
  cat
  echo
} >> "$(echo "$1" | sed -e 's@.@/@g')"
```

```{pipe="cat > show && chmod +x show"}
tee >(./hide "$@")
```

### Unstatified bags ###

Here's an unstratified implementation, which is encapsulated by a module called
`Data.Bag.Unstratified`. Every "element" of one of these `Bag` values is another
(unstratified) `Bag`, with arbitrary nesting allowed.

```{pipe="./hide Data.Bag.Unstratified"
module Data.Bag.Unstratified where

```

```{.haskell pipe="./show Data.Bag.Unstratified"}
data Bag = Empty
         | Insert Bag Bag
```

Every `Bag` is either `Empty` (an empty `Bag`), or `Insert foo bar`. The
latter is ambiguous about which is being inserted and which we're inserting
into: we'll declare that the first `Bag` (`foo`) is being inserted into the
second (`bar`). This is important if we want to operate on the *contents* of a
`Bag`; for example mapping a function over each element. Notice that we treat
the parts of an `Insert` differently: the first is given to `f` whilst the
second is given to `map f`:

```haskell
map :: (Bag -> Bag) -> Bag -> Bag
map f Empty = Empty
map f (Insert x xs) = Insert (f x) (map f xs)
```

This `map` function is a bit like that of a `Functor`, except that it's not
polymorphic. We can likewise make an analogue of `Applicative` by taking
cartesian products:

```haskell
join :: Bag -> Bag
join Empty                     = Empty
join (Insert Empty         ys) = join ys
join (Insert (Insert x xs) ys) = Insert x (join (Insert xs ys))

prop_joinEmpty = join Empty == Empty
prop_joinSingleton x = join (Insert x Empty) == x
prop_joinTwo x y = join (Insert x (Insert y Empty)) == add x y
```

``` haskell
product :: (Bag -> Bag -> Bag) -> Bag -> Bag -> Bag
product f Empty         y = Empty
product f (Insert x xs) y = add (map (f x) y) (product f xs y)
```

This relies on a definition of `add`, to combine the elements of one `Bag` with
those of another:

```haskell
add :: Bag -> Bag -> Bag
add Empty         y = y
add (Insert x xs) y = Insert x (add xs y)
```

If we use `add` as the combining function for `pruduct`, we get multiplication:

``` haskell
multiply :: Bag -> Bag -> Bag
multiply = product add
```

Here are some more basic definitions:

```haskell
zero = Empty
one = Insert zero zero
two = Insert zero one
alpha = Insert one Empty
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


### Stratified bags ###

```haskell
data SBag t = SEmpty
            | SInsert t (SBag t)
```

``` haskell
-- Zero
sZero :: SBag t
sZero = SEmpty

-- Nat
sOne :: SBag (SBag t)
sOne = SInsert sZero SEmpty
sTwo = SInsert sZero sOne

-- Poly
sAlpha = SInsert sOne SEmpty
```

```haskell
sinsertEq :: Eq t => t -> SBag t -> SBag t
sinsertEq x SEmpty                     = SInsert x SEmpty
sinsertEq x (SInsert y ys) | x == y    = SInsert y ys
sinsertEq x (SInsert y ys) | otherwise = SInsert y (sinsertEq x ys)
```

Since we want to allow bags of bags

```haskell
instance Eq t => Eq (SBag t) where
  (SInsert x xs) == (SInsert y ys) = (x, xs) == (y, ys)
  SEmpty         == SEmpty         = True
  _              == _              = False
```

```haskell
instance Ord t => Ord (SBag t) where
  compare (SInsert x xs) (SInsert y ys) = case compare x y of
    | EQ     -> compare xs ys
    | result -> result
  compare SEmpty  SEmpty = EQ
  compare SEmpty  _      = LT
  compare SInsert _      = GT
```

```haskell
sinsert :: Ord t => t -> SBag t -> SBag t
sinsert x SEmpty         = SInsert x SEmpty
sinsert x (SInsert y ys) = case compare x y of
  | EQ -> SInsert y ys
  | LT -> SInsert y (sinsert x ys)
  | GT -> SInsert x (SInsert y ys)
```
