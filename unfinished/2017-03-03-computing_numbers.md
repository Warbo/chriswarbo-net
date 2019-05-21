---
title: Computing Numbers
packages: [ "nix-shell" ]
---

This is a sequel to [an earlier blog post about computable numbers](
/blog/2017-02-27-listing_things.html). In that post I gave a simple suggestion
for computing numbers to unlimited precision using a Turing machine. Personally,
I much prefer language/algebraic approaches to computability, so I also wrote up
an alternative language-based approach that I find cleaner:

 - Similar to before, we will represent a number as two never-ending "streams"
   of bits, one on each side of the radix point.
 - Rather than fiddling around with binary tapes, interleaving, etc. we will
   keep everything abstract by data using [free variables](
   https://en.wikipedia.org/wiki/Free_variables_and_bound_variables). We will
   use 4 variables `l0`, `l1`, `r0` and `r1`.
 - Now we choose a universal programming language; I'm quite fond of
   [SK combinatory logic](
   https://en.wikipedia.org/wiki/SKI_combinator_calculus). SK programs are
   binary trees with leaves labelled either `S` or `K`. Computation is performed
   by rewriting parts of the tree which match one of the following rules:

```
  /\               x
 /\ y   becomes
K  x

   /\              /\
  /\ z  becomes   /  \
 /\ y            /\  /\
S  x            x z  y z
```

 - We can list all SK trees by starting with `S` and `K`, then using a similar
   procedure as Colin used to list the rationals: we list all pairs of natural
   numbers $(x, y)$ such that $x + y = 0$, followed by all pairs where
   $x + y = 1$, then $2$, and so on. For each $(x, y)$ pair, rather than turning
   them into rationals $\frac{x}{y}$ as Colin did, we instead construct a tree
   where the left and right children are the $x$th and $y$th trees from our
   list. We can see that this recursive definition is well-founded: we begin
   with the trees `S` and `K`, which don't require recursion, then each time we
   look one-step-further in the list, we end up adding many trees to the end; so
   our lookups can never 'overtake' our definitions.
 - For each tree `t` in our list, we replace it with a tree of the form:

```
    /\
   /\ r1
  /\ r0
 /\ l1
t  l0
```

 - We evaluate each of these trees to (weak head) normal form by following the
   above rewrite rules (this may not halt).
 - We interpret the results as follows:
  - We begin with a radix point "."; on its own this may be thought of as the
    number 0 (equivalent to …000.000…).
  - If the tree is a just a leaf, our interpretation is complete.
  - Otherwise, the tree is a node with two children `x` and `y`.
  - If `x` is the variable `l0`, this tree represents the same number as `y`,
    but with a 0 inserted to the left of the radix point. For example, if `y`
    represents a number like …00111.110011…, then this tree represents
    …001110.110011….
  - Likewise if `x` is the variable `l1`, a 1 is insert to the left of the radix
    point.
  - Similarly, `r0` and `r1` insert a 0 or 1 to the *right* of the radix point,
    respectively.
  - If `x` has any other form, it is ignored; this tree represents the same
    number as `y`.

I've made a quick implementation of this in Haskell; here's the beginning of the
list:

```{pipe="cat > runner"}
#!/usr/bin/env bash
nix-shell --run runhaskell \
          -p '(haskellPackages.ghcWithPackages (h: [ h.control-monad-omega ]))'
```

```{.haskell pipe="sh ./runner"}
{-# LANGUAGE BangPatterns, MonadComprehensions #-}
module X where

import Control.Monad
import Control.Monad.Omega
import Data.List
import Data.Maybe

data Comb = S | K | Node Comb Comb | V Var deriving (Eq, Show)

data Var = Pair | Zero | One deriving (Eq, Show)

step (Node (Node K x) y)          = (True, x)
step (Node (Node (Node S x) y) z) = (True, Node (Node x z) (Node y z))
step (Node x y)                   = case step x of
                                         (True, x') -> (True, Node x' y)
                                         (False, _) -> Node x <$> step y
step x                            = (False, x)

reduce 0 x = Nothing
reduce n x = case step x of
                 (True, x') -> reduce (n-1) x'
                 (False, _) -> Just x

trees = S : K : runOmega [Node x y | x <- each trees, y <- each trees]

readTree n t = do t' <- reduce n (Node t (V Pair))
                  case t' of
                       Node (Node (V Pair) l) r -> readStreams n l r
                       _                        -> Nothing

readStreams n l r = do l' <- readStream n l
                       r' <- readStream n r
                       pure (l', r')

readStream n = go [] 8
  where go !acc 0 _ = Just acc
        go !acc m x = do t' <- reduce n (Node (Node x (V Zero)) (V One))
                         case t' of
                              Node (V Zero) rest -> go (acc ++ [0]) (m-1) rest
                              Node (V One)  rest -> go (acc ++ [1]) (m-1) rest
                              _                  -> Just acc

evaledTrees = map (readTree 10) trees

bitsToNum :: ([Int], [Int]) -> Double
bitsToNum (l, r) = parseWhole 0 1 (reverse l) + parseFrac 0 0.5 r
  where parseWhole !n !power []     = n
        parseWhole !n !power (0:xs) = parseWhole  n          (power * 2) xs
        parseWhole !n !power (1:xs) = parseWhole (n + power) (power * 2) xs

        parseFrac  !n !power []     = n
        parseFrac  !n !power (0:xs) = parseFrac   n          (power / 2) xs
        parseFrac  !n !power (1:xs) = parseFrac  (n + power) (power / 2) xs

nubble = go []
  where go seen []     = []
        go seen (x:xs) = if x `elem` seen
                            then     go    seen  xs
                            else x : go (x:seen) xs

-- FIXME: We're only taking 1 since even taking 5 ends up blowing the memory
-- after a while. The problem is that we're throwing away so many programs; I'd
-- like to think of a more direct encoding, but haven't come up with one that's
-- functional (e.g. we could read the S and K occurrences in post-order, but
-- that violates things like beta-equality).
render = intercalate ", " . take 1 . nubble .
         -- Turn into a list of strings of numbers
         map (show . bitsToNum) .
         -- Discard anything that failed to normalise
         catMaybes

main = putStr . render $ evaledTrees
```
