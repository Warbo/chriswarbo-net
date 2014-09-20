---
title: Naturals Search
---
I came across this interesting pattern a while ago, while taking
my first substantial steps in Haskell, and thought I'd blog it.

It's a logarithmic time search over the Naturals, for any
predicate where we can tell if a number's too big. I use it
here to implement square-root:

We define an `ITree`{.haskell} as a tree of integers. Each node can either
be a leaf, containing an `Integer`{.haskell}, or a node containing an
`Integer`{.haskell} and two sub-trees:

```haskell
data ITree = IL Integer | IN Integer ITree ITree
```

This tells us how to show an `ITree`{.haskell} so that we can use
printf-debugging:

```haskell
instance Show ITree where
  show (IL x) = "(" ++ show x ++ ")"
  show (IN x l r) = "(" ++ show x ++ " " ++ show l ++ " " ++ show r ++ ")"
```

This is an `ITree`{.haskell}, and in fact is the only one we need.
It is constructed lazily, and has the following shape:

```
  1
 / \\
1   2
   / \\
  2   4
     / \\
    3   \\
   / \\   \\
  3  4    \\
           8
          / \\
         6   \\
        / \\   .
       /   \\   .
      5     7   .
     / \\   / \\
    5   6 7   8
```

The idea is that the right-most branches count up in powers of
`2`{.haskell} forever. The branches coming off on the left contain a leaf
for each of the numbers between this power of `2`{.haskell} and the previous
(eg. the branch coming off `8`{.haskell} contains leaves for `5`{.haskell}, `6`{.haskell}, `7`{.haskell} and `8`{.haskell}).
These are spread out by splitting into above/below the average
value which we stick on the node.

```haskell
iTree = let biTree l h | l == h    = IL l
                       | otherwise = let m = avgI l h in
                                         IN m (biTree l m) (biTree (m + 1) h)
            from x = IN (2^x) (biTree (2^(x-1) + 1) (2^x)) (from (x+1)) in
            IN 1 (IL 1) $ IN 2 (IL 2) $ from 2
```

This function finds the largest number which doesn't trigger the
given `tooHigh`{.haskell} function. It does this by traversing `iTree`{.haskell}: it
keeps going right until `tooHigh`{.haskell} becomes `True`{.haskell}, then it snakes down
the left sub-tree, hugging the `tooHigh`{.haskell} boundary until it hits a
leaf.

```haskell
find tooHigh = let f (IN x l r) = if tooHigh x
                                     then f l
                                     else f r
                   f (IL x)     = x in
                   f iTree
```

This is a simple example which uses `iTree`{.haskell} to find the integer
square root of a number.

```haskell
sqrtI :: Integer -> Integer
sqrtI n | n <= 1    = 1
sqrtI n | otherwise = find $ (> n) . (^ 2)
```

Here's a `QuickCheck`{.haskell} property to verify that it's correct.

```haskell
prop_sqrtI n = let root = sqrtI n
                   lower = (root - 1)^2
                   higher = (root + 1)^2 in
                   n > 0 ==> lower <= n && higher >= n
```

Anyone know what this `iTree`{.haskell} structure is called?
