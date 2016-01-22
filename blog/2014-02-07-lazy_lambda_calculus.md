---
title: Lazy Lambda Calculus
---

```{pipe="tee -a 1.hs > /dev/null"}
import Test.SmallCheck

```

Recently I've been playing with meta-programming in [lambda calculus] [1] (originally I tried using [combinatory logic] [2] but the combinators rapidly became too large to understand :( ). For this, I wanted an LC implementation with the following characteristics:

 - Implemented in Haskell
 - Terms which I can pattern-match against (ie. not just regular functions)
 - [De Bruijn indices] [3]
 - A non-diverging (ie. [co-recursive] [4]) evaluation function

[1]: http://en.wikipedia.org/wiki/Lambda_calculus
[2]: http://en.wikipedia.org/wiki/Combinatory_logic
[3]: http://en.wikipedia.org/wiki/De_Bruijn_index
[4]: http://en.wikipedia.org/wiki/Corecursion

I tried implementing this myself, and here's the infrastructure I built to do it:

```{.haskell pipe="tee -a 1.hs"}
-- Peano-style natural numbers
data Nat = Z
         | S Nat

-- Co-inductive wrapper to allow undefined output (infinite Laters)
data Partial a = Now a
               | Later (Partial a)

-- We can map functions over partial results
instance Functor Partial where
  fmap f (Now   x) = Now        (f x)
  fmap f (Later x) = Later (fmap f x)

-- Partiality is a monad
instance Monad Partial where
  return = Now  -- Immediate value
  (Now   x) >>= f = Later (f x)
  (Later x) >>= f = Later (x >>= f)

-- LC terms
data Term a = Const a       -- Opaque Haskell values
            | Var Nat       -- De Bruijn index
            | Lam (Term a)  -- Anonymous function
            | Term :@ Term  -- Function application

```

My first attempt hit problems with [closure] [5]. Specifically, I was trying to make an evaluation function with Terms as input **and** output, but this gave me nowhere to store the associated environment:

[5]: http://en.wikipedia.org/wiki/Closure_%28computer_programming%29

```{.haskell pipe="tee -a 1.hs"}
-- (Broken) evaluation function
eval' :: [Partial (Term a)] -> Term a -> Partial (Term a)
eval' (e:es) (Var  Z)    = e
eval' (e:es) (Var (S n)) = eval' es (Var n)
eval'  e     (l :@ r)    = let ev = eval' e in
                           do l' <- ev l
                              case l' of
                                Lam f -> Later (eval' (ev r : e) f)
                                _     -> error "Can only apply Lams"
eval'  e      t          = t

eval = eval' []

```

To see why this is incorrect, we can use [SmallCheck] [6]. Let's check the property that closed terms (ie. those with no free variables) remain closed after evaluation:

[6]: http://hackage.haskell.org/package/smallcheck

```{pipe="tee -a 1.hs > /dev/null"}```

```haskell
-- Predicate to see if a Term has fewer than n free variables
closed' n (Var m)  = m < n
closed' n (Lam f)  = closed' (S n) f
closed' n (l :@ r) = closed' n l && closed' n r
closed' _ _        = True

closed = closed' 0

-- Helper functions

-- Force a result in n steps, or else fail
force :: Nat -> Partial a -> Maybe a
force  Z     _        = Nothing
force (S n) (Now   x) = Just x
force (S n) (Later x) = force n x

-- A conservative decision procedure
trueIn n x = case force n x of
               Just b  -> b
               Nothing -> False

-- A lax decision procedure
notFalseIn n x = case force n x of
                   Just b  -> b
                   Nothing -> True

-- Our test
closedTest n x = closed x ==> notFalseIn n (fmap closed (eval x)))
```

We run this and get the following result:

```haskell
depthCheck 5 closedTest
LSC: Counterexample found after 49175 tests.
Var 0: S (S _)
Var 1: Lam (Lam (Var 1)) :@ Lam (Lam (Lam (Const _)))
*** Exception: ExitFailure 1
```

This tells us that it found a counterexample when `n` is at least 2 and `x` is `Lam (Lam (Var 1)) :@ Lam (Lam (Lam (Const _)))` (for any wildcard '_'). These incomplete results are thanks to [Lazy SmallCheck] [8], which tries to identify exactly which parts of the input cause the failure.

[7]: https://github.com/UoYCS-plasma/LazySmallCheck2012

Why does this fail? Well, when this Term is evaluated, we get `Later (Now (Lam (Var 1)))`. There are two Partial wrappers around the Term, which is why the error only appears after forcing at least 2 steps. The Term itself, `Lam (Var 1)`, is not closed since a single Lam only gives us access to `Var 0`. This should never happen, so what's gone wrong?

The input contains an 'outer' function `Lam (Lam (Var 1))` which is returning an 'inner' function, which in turn returns the 'outer' function's argument. The outer function is applied to another Term, so we get back the inner function. However, this inner function needs some way to reference the outer function's argument, but our Term datatype doesn't give us anywhere to store this reference. Instead, we're forced to throw it away, so the `Var 1` Term is left 'dangling' as a free variable, which violates our property.

I tried keeping track of the environment inside the constructors of Term, but everything got very messy very quickly, so I gave up and Googled around for a solution. I found [this presentation] [8] which contains an interpreter for a quite complex language, featuring native booleans and integers, primitive operations on those types, `while` and `until` loops, explicit recursion, etc. Most of this is unnecessary for what I want, but it does feature lambdas, application, variables and an evaluator which uses `Partial` (although the presentation calls it `Delay`). Here's a stripped-down version, with the extraneous stuff removed:

[8]: http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/22/slides/tarmo.pdf

```haskell
type Name = String

data Term = Var Name
          | Lam Name Term
          | Term :@ Term

data Val = F (Val -> Partial Val)

type Env = [(Name, Val)]

eval' :: Term -> Env -> Partial Val
eval' (Var   x)   env = return (unsafelookup x env)
eval' (Lam x f) env = return (F (\\a -> eval' f (update x a env)))
eval' (f :@  x)  env = do F f' <- eval' f env
                         x'   <- eval' x env
                         f' x'
```

One problem that's immediately apparent is that `Term` has lost the `Const` constructor. Since Const values are opaque to the interpreter, they're simple enough to add back in:

```haskell
type Name = String

data Term a = Var Name
            | Lam Name (Term a)
            | Term a :@ Term a
            | Const a

data Val a = F (Val a -> Partial (Val a))
           | C a

type Env a = [(Name, Val a)]

eval' :: Term a -> Env a -> Partial (Val a)
eval' (Const x) env = return (C x)
eval' (Var   x) env = return (unsafelookup x env)
eval' (Lam x f) env = return (F (\\a -> eval' f (update x a env)))
eval' (f :@  x) env = do F f' <- eval' f env
                         x'   <- eval' x env
                         f' x'
```

Now let's switch to de Bruijn indices instead of String variables:

```haskell
data Term a = Var Nat
          | Lam (Term a)
          | Term a :@ Term a
          | Const a

data Val a = F (Val a -> Partial (Val a))
           | C a

type Env a = [Val a]

eval' :: Term a -> Env a -> Partial (Val a)
eval' (Const x) env = return (C x)
eval' (Var   n) env = let Just x = lookUp n env in return x
eval' (Lam   f) env = return (F (\\a -> eval' f (a:env)))
eval' (f :@  x) env = do F f' <- eval' f env
                         x'   <- eval' x env
                         f' x'
```

Now this is looking very similar to my previous, broken solution. What's the difference? Rather than evaluating Terms into Terms, we evaluate them into "Vals", which can contain Haskell functions. These Haskell functions can be closures, with access to whichever `env` variable was in scope when they were defined; hence we're implementing closures in LC by using Haskell's own closures!

Unfortunately there is actually a bug in this interpreter, which prevents it satisfying the properties I want. Specifically, it never uses `Later` to delay a result! This means that, even though it builds a Partial result, it will still diverge because it tries to put everything in a `Now` constructor. This makes it dangerous to evaluate arbitrary Terms, since the evaluator may get caught in a loop. What we need to do is add a `Later` every time the evaluator performs beta-reduction, ie. in the `f :@ x` case:

```haskell
eval' :: Term a -> Env a -> Partial (Val a)
eval' (Const x) env = return (C x)
eval' (Var   n) env = let Just x = lookUp n env in return x
eval' (Lam   f) env = return (F (\\a -> eval' f (a:env)))
eval' (f :@  x) env = do F f' <- eval' f env
                         x'   <- eval' x env
                         Later (f' x')
```

With this addition, the interpreter is now safe to run on arbitrary Terms, which allows us to use property-checking tools like SmallCheck and [QuickCheck] [9].

[9]: http://en.wikipedia.org/wiki/QuickCheck

In fact we can make another change, to turn this from a [call-by-value] [10] interpreter into a [call-by-need] [11] interpreter. When we evaluate an application `f :@ x`, we are currently evaluating `f` to get a closure `F f'` then we are evaluating `x` to get a Val `x'`, then we are using application `f' x'` to get our result. This forces us to evaluate our argument `x`, even if it's never used. If `x` causes an infinite loop, then our program is guaranteed to get stuck in a loop.

[10]: http://en.wikipedia.org/wiki/Call_by_value#Call_by_value
[11]: http://en.wikipedia.org/wiki/Call_by_value#Call_by_need

Instead, we can use call-by-need, like Haskell, such that we only evaluate `x` if it's needed. To do this we need to get rid of the line `x' <- eval' x env`, since that will wait for the evaluation of `x` to finish. We still need to evaluate `x`, and we still need to do it in the context of the correct `env`, so what can we do? The solution is to put *Partial* Vals in the environment, rather than fully-evaluated ones. Such an interpreter looks like this:

```haskell
-- Environments now contain Partial Vals
type Env a = [Partial (Val a)]

-- Function Vals now accept Partial arguments
data Val a = F (Partial (Val a) -> Partial (Val a))
           | C a

-- Evaluation now takes an Environment of Partial Vals
eval' :: Term a -> Env a -> Partial (Val a)
eval' (Const c) env = Now (C c)
eval' (Var   n) env = let Just x = lookUp env n in x
eval' (Lam   f) env = Now (F (\\a -> eval' f (a:env)))
eval' (f :@  x) env = do F f' <- eval' f env
                      Later (f' (eval' x env))
```

Now we will only get trapped in infinite loops when they're unavoidable, and even then it won't cause Haskell to get stuck, since we're using the Partial monad.

Now we can use SmallCheck to test some properties of our interpreter. The ones I've tried so far, without issue, are:

```haskell
-- Helpers

-- Force n steps of evaluation
evalN n x = force n (eval x)

-- Apply a function Val to a constant
($$) (F f) x = f (Now (C x))

-- Conservative and lax predicates for checking that y evaluates to x
equalIn   n x y = trueIn     n (fmap (== C x) (eval y))
notDiffIn n x y = notFalseIn n (fmap (== C x) (eval y))

-- Handy Terms

omega = Lam (Var 0 :@ Var 0) :@ Lam (Var 0 :@ Var 0)

yComb = Lam (Lam (Var 1 :@ (Var 0 :@ Var 0)) :@
             Lam (Var 1 :@ (Var 0 :@ Var 0)))

-- Tests

evalCoterminates x n = let x' = evalN n x in
                       closed x ==> isJust x' || isNothing x'

omegaDiverges n = evalN n (Lam ((0 :@ 0) :@ (0 :@ 0))) == Nothing

evalApp     x = trueIn  1 (fmap (== C x) (eval (Lam 0) >>= ($$ x)))

evalSteps   x = equalIn 3 x (Lam 0 :@ Lam 0 :@ Const x)

idWorks     x = equalIn 2 x (i :@ Const x)

yTerminates x = equalIn 7 x (yComb :@ Lam (Lam 0) :@ Const x)

trueWorks  n x y = let t = Lam (Lam (Var 1)) :@ Const x :@ y in
                   closed t ==> notDiffIn n x t

falseWorks n x y = let f = Lam (Lam (Var 0)) :@ x :@ Const y in
                   closed f ==> notDiffIn n y f

zeroWorks  n x y = let z = Lam (Lam (Var 1)) :@ Const x :@ y in
                   closed z ==> notDiffIn n x z
zFalse x = equalIn 5 x (zComb :@ Lam (Lam (Var 0)) :@ Const x)
```

I'm reasonably confident that this interpreter is correct, but if anyone can spot a problem (or an improvement) then please let me know!

As usual, the code for this is living in [Git] [12].

[12]: http://chriswarbo.net/git/lazy-lambda-calculus
