---
title: A Framework for Self-Improving Code
---

Since Haskell functions are opaque (we can't pattern-match them), we'll define a simple Lambda Calculus to represent our functions instead (this was explained in [a previous post](/blog/2014-02-07-lazy_lambda_calculus.html)):

```{.haskell pipe="sh"}
git clone ...
cat ...
```


```{pipe="cat > append"}
#!/bin/sh
tee -a code.hs
echo "" >> code.hs
```

```{pipe="sh"}
chmod +x append
```

```{pipe="./append > /dev/null"}
import Test.QuickCheck
```

```{.haskell pipe="./append"}
-- Lambda Calculus terms
data Term a = Var Nat
            | Lam (Term a)
            | App (Term a) (Term a)
            | Const a

-- De Bruijn indices
data Nat = Z | S Nat

lookUp :: [a] -> Nat -> Maybe a
lookUp xs n = let toInt Z     = 0
                  toInt (S m) = 1 + toInt m
                  n'          = toInt n
               in if n' < length xs
                     then Just (xs !! n')
                     else Nothing

-- Compiled closures
data Val a = F (Partial (Val a) -> Partial (Val a))
           | C a

-- Environment mapping indices to terms
type Env a = [Partial (Val a)]

-- Lambda calculus evaluator
eval' :: Term a -> Env a -> Partial (Val a)
eval' (Const c) env = Now (C c)
eval' (Var   n) env = let Just x = lookUp env n in x
eval' (Lam   f) env = Now (F (\a -> eval' f (a:env)))
eval' (App f x) env = do F f' <- eval' f env
                         Later (f' (eval' x env))

eval x = eval' x []

-- Turn the general recursion of Lambda Calculus into co-recursion, to avoid killing Haskell
data Partial a = Now a | Later (Partial a)

instance Functor Partial where
  fmap f (Now   x) = Now        (f x)
  fmap f (Later x) = Later (fmap f x)

instance Monad Partial where
  return = Now
  (Now   x) >>= f = Later (f x)
  (Later x) >>= f = Later (x >>= f)
```

```{pipe="./append"}
instance Eq a => Eq (Val a) where
  C x == C y = x == y
  _   == _   = False

instance Arbitrary a => Arbitrary (Term a) where
  arbitrary = Var   <$> arbitrary
              App   <$> arbitrary <*> arbitrary
              Lam   <$> arbitrary
              Const <$> arbitrary

extractTo :: Int -> Partial a -> Maybe a
extractTo _ (Now   x) = Just x
extractTo n _ | n < 1 = Nothing
extractTo n (Later x) = extractTo (n-1) x

evalFor n t = extractTo n (eval t)

normalIn :: Eq a => Int -> Term a -> Bool
normalIn n t = evalFor n t /= Nothing

testId :: Eq a => Int -> Term a -> Bool
testId n t = not (normalIn n t) || evalFor (n * n) (App (Lam (Var Z)) t) == evalFor n t
```

```{pipe="ghci -v0"}
:load code.hs
quickCheck testId
```
