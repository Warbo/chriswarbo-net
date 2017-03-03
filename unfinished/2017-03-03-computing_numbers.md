---
title: Computing Numbers
---

Personally, I much prefer language/algebraic approaches to computability. Here's
how I'd go about it:

 - Similar to above, we will represent a number as two never-ending "streams" of
   bits, one on each side of the radix point.
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

```{pipe="nix-shell -p '(haskellPackages.ghcWithPackages (h: [ h.control-monad-omega ])' --run runhaskell"}
{-# LANGUAGE MonadComprehensions #-}
module X where

import Control.Monad
import Control.Monad.Omega

data Comb = S | K | Node Comb Comb | V Var deriving (Show, Eq)

data Var = L0 | L1 | R0 | R1 deriving (Eq, Show)

-- Tells us whether or not an expression *could* be reduced, but doesn't
-- actually do it; this function always halts
reducible x = redex x || case x of
  K                          -> False
  S                          -> False
  V _                        -> False
  Node x y                   -> reducible x || reducible y

redex (Node (Node K a) b)          = True
redex (Node (Node (Node S a) b) c) = True
redex _                            = False

reduce 0 x |      reducible x     = Nothing
reduce n x | not (reducible x)    = Just x
reduce n x |      redex     x     = reduce (n-1) (step x)
reduce n (Node x y) | reducible x = do x' <- reduce n x
                                       return (reduce (n-1) (Node x' y))
reduce n (Node x y) | reducible y = do y' <- reduce n y
                                       return (reduce (n-1) (Node x y'))

step (Node (Node K x) y)          = x
step (Node (Node (Node S x) y) z) = Node (Node x z) (Node y z)
step _ = error "Can't step"

whnf _ S                       = Just S
whnf _ K                       = Just K
whnf _ (V v)                   = Just (V v)
whnf 0 (App x y) | reducible x = Nothing
whnf n (App x y) | reducible x = do x' <- reduce n x
                                    if redex (App x' y)
                                       then whnf (n-1) (step (App x' y))
                                       else Just (App x' y)
whnf n (App x y) | otherwise   = Just (App x y)

spine n S = []
spine n K = []
spine n (App x y) = x : case whnf n y of
                             Nothing -> []
                             Just y' -> spine (n-1) y'

readNum n (left, right) []        = (left, right)
readNum n (left, right) (V L0:xs) = readNum (n-1) (0:left, right) xs
readNum n (left, right) (V L1:xs) = readNum (n-1) (1:left, right) xs
readNum n (left, right) (V R0:xs) = readNum (n-1) (left, 0:right) xs
readNum n (left, right) (V R1:xs) = readNum (n-1) (left, 1:right) xs
readNum n (left, right) (_:xs)    = readNum (n-1) (left, right)   xs

trees = S : K : runOmega [Node x y | x <- each trees, y <- each trees]

applied = [Node (Node (Node (Node t (V L0)) (V L1)) (V R0)) R1 | t <- trees]

evaledTrees = map (reduce 1000) applied
```
