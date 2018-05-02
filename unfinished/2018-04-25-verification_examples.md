---
title: Verification Examples
---


```
> quickSpec (fun3 "leftPad" leftPad)
== API ==
-- functions --
leftPad :: Char -> Int -> [Char] -> [Char]

-- WARNING: the following types are uninhabited --
Int (used in leftPad)
Char (used in leftPad)

-- WARNING: there are no variables of the following types; consider adding some --
[Char]

== Testing ==
Depth 1: 1 terms, 0 tests, 0 evaluations, 1 classes, 0 raw equations.
Depth 2: 1 terms, 0 tests, 0 evaluations, 1 classes, 0 raw equations.
Depth 3: 1 terms, 0 tests, 0 evaluations, 1 classes, 0 raw equations.
0 raw equations; 1 terms in universe.
```


```
sig :: Sig
sig = signature [
    fun3 "leftPad" leftPad
  , vars ["c1", "c2", "c3"] (undefined :: Char)
  , vars ["i1", "i2", "i3"] (undefined :: Int)
  , vars ["s1", "s2", "s3"] (undefined :: String)
  ]
```

```
> quickSpec sig
== API ==
-- functions --
leftPad :: Char -> Int -> [Char] -> [Char]

-- variables --
n1, n2, n3 :: Int
c1, c2, c3 :: Char
s1, s2, s3 :: [Char]

== Testing ==
Depth 1: 10 terms, 2 tests, 13 evaluations, 10 classes, 0 raw equations.
Depth 2: 37 terms, 5 tests, 122 evaluations, 37 classes, 0 raw equations.
Depth 3: 280 terms, 500 tests, 81737 evaluations, 172 classes, 108 raw equations.
108 raw equations; 172 terms in universe.

== Equations about leftPad ==
  1: leftPad c1 n1 (leftPad c2 n1 s1) == leftPad c2 n1 s1
  2: leftPad c1 n1 (leftPad c1 n2 s1) == leftPad c1 n2 (leftPad c1 n1 s1)
```
