---
title: Purely-Functional Self-Modifying Code
---

```{pipe="tee tangle > /dev/null"}
#!/bin/sh
tee -a code.hs
echo "" >> code.hs
```

```{pipe="sh > /dev/null"}
chmod +x tangle
```

```{.haskell pipe="tee -a code.hs"}
{-# LANGUAGE RankNTypes, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric #-}
module Encoding where

--import Control.Monad.State
import Control.Applicative
import Data.Data
import Data.Typeable
import GHC.Generics
import Test.QuickCheck hiding (Testable)
import Test.SmallCheck hiding (Testable)
import Test.SmallCheck.Drivers
import Test.SmallCheck.Series hiding ((><))
import qualified Test.QuickCheck as Q (Testable)
import qualified Test.SmallCheck as S (Testable)

liftM f x = x >>= return . f
liftM2 f x y = do x' <- x
                  y' <- y
                  return (f x' y')

-- Runs tests with output suitable for the Web
check' :: (Q.Testable t, S.Testable IO t) => Int -> t -> IO String
check' n p = do s <- smallCheckM n p
                q <- quickCheckWithResult stdArgs {chatty = False} p
                return $ concat ["SmallCheck: ", maybe "OK" ppFailure s,
                                 "\nQuickCheck: ", output q]

check :: (Q.Testable t, S.Testable IO t) => t -> IO String
check = check' 5
```

## Introduction ##

One of my main research interests is self-improving code. Clearly such algorithms must be self-modifying, or else they'd never be able to perform improvements. The trouble with self-modifying code is that it tends to be implemented imperatively. Since imperative code is difficult to reason about, this makes it hard to restrict modifications to only those which produce improvement.

In this post I'll present one way of implementing self-modification in a purely functional way, which we can then verify as self-improving.

## Code As Data ##

Since pure functions must take all input via function arguments, and since we're going to modify our own code, we must therefore take our own code as a function argument. To do this we'll need some way to represent our code as data, since we'll be working in a lambda calculus where functions themselves are black-boxes. Imperatively this can be done by providing an access mechanism to mutable memory, but we can do better than that.

Outside the world of self-modification, the usual way to represent code as data is via an Abstract Syntax Tree. ASTs are quite straightforward to define, especially in nice languages like Haskell; however, ASTs *of* Haskell are complicated, so I'll stick to representing ASTs *of* Lambda Calculus *in* Haskell:

```{.haskell pipe="tee -a code.hs"}
-- Peano Naturals
data Var = Z | S Var deriving (Eq)

instance Num Var where
  fromInteger 0 = Z
  fromInteger n = S (abs (fromInteger n))

instance Enum Var where

instance Ord Var where

instance Real Var where

instance Integral Var where
  toInteger  Z    = 0
  toInteger (S n) = 1 + toInteger n

var :: Integer -> Var
var = fromInteger

data AST = F (AST -> AST)  -- Functions from ASTs to ASTs
         | A AST AST       -- Applying one AST to another
         | V Var           -- Free variable
     deriving Generic

(!) = A  -- Infix application

instance Show AST where
  show (F _)   = "λ"
  show (A l r) = "(" ++ show l ++ ")(" ++ show r ++ ")"
  show (V n)   = show (toInteger n)

instance Eq AST where
  F _   == F _   = undefined
  A a b == A c d = a == c && b == d
  V a   == V b   = a == b
  _     == _     = False
```

I'll use QuickCheck and SmallCheck to test things as I go, both of which require AST generators. There are a few considerations when generating ASTs:

 - To avoid infinite loops during evaluation, we should only generate terms which are in normal form (ie. containing no "A (F _) _" terms).
 - To avoid generating infinite terms, we must have a finite expected depth:
   - "F" is a terminal:
     - We generate a single AST -> AST function and stop.
     - The expected depth of an "F" is 1.
   - "V" is a terminal:
     - We generate a single Int and stop.
     - The expected depth of a "V" is 1.
   - "A" is a non-terminal:
     - We need 1 arbitrary AST for its second parameter, increasing our depth by 1.
     - We need a "V" or "A" AST for the first parameter, which guarantees a subsequent depth increase.
     - Trees of "A"s will have infinite expected depth if the branching factor (the expected number of sub-terms) is more than the probability of getting an "F" or "V".
 - Each "F" requires a function, of type "AST -> AST".
   - We satisfy this for QuickCheck using "CoArbitrary".
   - We satisfy this for SmallCheck using "CoSerial".

```{.haskell pipe="tee -a code.hs"}
-- Arbitrary AST
instance Arbitrary AST where
  arbitrary = let f  = liftM  F    arbitrary
                  a  = liftM2 A a' arbitrary
                  a' = liftM  noF  arbitrary
                  v  = liftM  F    arbitrary in
                  frequency [(10, f), (1, a), (2, v)]

  -- Tries to shrink counterexamples
  shrink x = case x of
                  F _   -> []
                  A l r -> [l, r]

-- We need Arbitrary Vars too
instance Arbitrary Var where
  arbitrary = oneof [return Z, S <$> arbitrary]
  shrink  Z    = []
  shrink (S n) = [n]

instance Monad m => Serial m Var where
  series = cons0 Z \/ cons1 S

instance Monad m => CoSerial m Var where

-- Enumerating AST
instance Monad m => Serial m AST where
  series = cons1 F \/ cons1 V \/ cons2 (\x -> A (noF x))

-- Arbitrary AST -> AST
instance CoArbitrary AST where
  coarbitrary (F f)   = variant 0 . (coarbitrary f)
  coarbitrary (A l r) = variant 1 . (coarbitrary l >< coarbitrary r)

-- Enumerating AST -> AST
instance Monad m => CoSerial m AST

-- Uncallable terms, to restrict the above to normal forms only
data NoFunc = An NoFunc AST | Vn Var deriving Generic

instance Arbitrary NoFunc where
  arbitrary = frequency [(1, liftM2 An arbitrary arbitrary),
                         (2, liftM  Vn arbitrary)]

instance Monad m => Serial m NoFunc

noF x = case x of
             An l r -> A (noF l) r
             Vn n   -> V n
```

Now we can check that only normal terms are generated:

```{.haskell pipe="tee -a code.hs"}
normal x = case x of
                F _       -> True
                V _       -> True
                A (F _) _ -> False
                A l r     -> normal l && normal r
```

```
{pipe="ghci"}
:load code.hs
check normal
```

Of course, an AST isn't much use if we can't run it. Here's a corresponding evaluation function:

```{.haskell pipe="tee -a code.hs"}
eval e = let app (F f) x = eval (f x)  -- Call a function's body
             app    f  x = f ! x       -- Don't apply non-functions
         in case e of
                 V   v -> V v             -- Don't reduce free variables
                 F   b -> F b             -- Functions don't reduce
                 A l r -> app (eval l) r  -- Eval the applicand and apply
```

Note that this function is not total, ie. it may not halt. This is why we only generate normal ASTs for testing.

## Morgensen-Scott Encoding ##

It's all well and good having Lambda Calculus ASTs in Haskell, but what we really need are Lambda Calculus ASTs in Lambda Calculus. This may seem difficult, since Lambda Calculus only has functions, not data. We can work around this using a clever scheme known as Morgensen-Scott encoding.

The first thing to note is that, since everything in LC is a function and functions are black boxes, there are no case expressions; the only way to distinguish between two values (functions) is to apply them to some arguments and see what happens.

The simplest distinction we can make is between the booleans 'true' and 'false'; how might we achieve this in LC? Recall that in Haskell the booleans look like this:

```haskell
  data Boolean = True | False
```

Booleans are only useful when passed to 2-branch 'case' statements, so we might as well combine these concepts and pass the branches straight to our booleans:

```{.haskell pipe="tee -a code.hs"}
true2, false2 :: Boolean2
true2  x y = x  -- Accept 2 branches, return the first
false2 x y = y  -- Accept 2 branches, return the second

-- Based on the above, Boolean2 must be the following
type Boolean2 = forall a. a -> a -> a

-- Given these definitions, "if" becomes trivial
if2 :: Boolean2 -> a -> a -> a
if2 cond branch1 branch2 = cond branch1 branch2
```

Since LC is un(i)typed we can ignore the Boolean2 type, which just leaves us with functions, which are simple to define in LC:

```{.haskell pipe="tee -a code.hs"}
true3, false3, if3 :: AST

--          \x.      \y.   x
true3  = F (\x -> F (\y -> x))

--          \x.      \y.   y
false3 = F (\x -> F (\y -> y))

--          \c.      \b1.      \b2.        c b1  b2
if3    = F (\c -> F (\b1 -> F (\b2 -> c ! b1 ! b2)))
```

As you can see, values of a type with two constructors can be represented by functions taking two arguments. The first argument is used by values built by the first constructor and the second argument is used by those built by the second constructor. This principle can be extended arbitrarily, so values of a type with N constructors can be represented by N-ary functions.

The next issue we need to deal with is constructors which take arguments. For example:

```haskell
  data Maybe a = Nothing | Just a
```

We can distinguish between the "Nothing" and "Just" constructors just like we did with "True" and "False"; in fact "Nothing" turns out to be equivalent to "True"! In the case of "Just", we don't immediately return the second argument like we did for "False"; instead, we call it as a function, passing in the value which was wrapped by "Just":

```{.haskell pipe="tee -a code.hs"}
nothing2 :: Maybe2 a
nothing2 x y = x

just2 :: a -> Maybe2 a
just2 a x y = y a

-- Based on the above definitions, this must be their type
type Maybe2 a = forall b. b -> (a -> b) -> b
```

For LC we can again ignore the types and just implement the functions:

```{.haskell pipe="tee -a code.hs"}
nothing3, just3 :: AST

--            \x.      \y.   x
nothing3 = F (\x -> F (\y -> x))

--         \a.      \x.      \y.     y a
just3 = F (\a -> F (\x -> F (\y -> y ! a)))
```

Notice that we can't compare "Nothing" and "Just" directly, since they have different types. We can only compare "Nothing" with "Just a" for some value of "a", which we pass in before the constructor-selected arguments (ie. the "x" and "y" which distinguish "Nothing" from "Just a").

Again, this principle of passing along constructor arguments can be scaled up to arbitrary arity. A constructor with N arguments can be represented by accepting those N arguments then passing them to the argument representing the constructor.

With these two techniques in hand, we can model our AST type itself using nothing but functions:

```{.haskell pipe="tee -a code.hs"}
f2 :: (AST2 -> AST2) -> AST2
f2 f = AST2 (\x y z -> x (f2 f))

a2 :: AST2 -> AST2 -> AST2
a2 l r = AST2 (\x y z -> y l r)

v2 :: AST2 -> AST2
v2 v = AST2 (\x y z -> z v)

-- The type of the above ASTs; the complexity is due to the recursion
newtype AST2 = AST2 { getAST2 :: (AST2 -> AST2)         ->
                                 (AST2 -> AST2 -> AST2) ->
                                 (AST2 -> AST2)         ->
                                  AST2 }
```

Again we can ignore the (more complicated) type and keep the functions. We can clearly see that the f2, a2 and v2 constructor functions match the argument types of the AST2 type, which gives us confidence that we're on the right track. Let's see how these functions look as LC terms:

```{.haskell pipe="tee -a code.hs"}
f3, a3, v3 :: AST

--               \f.      \x.      \y.      \z.   x   f
f3 =          F (\f -> F (\x -> F (\y -> F (\z -> x ! f))))

--      \a.      \b.      \x.      \y.      \z.   y   a   b
a3 = F (\a -> F (\b -> F (\x -> F (\y -> F (\z -> y ! a ! b)))))

--               \v.      \x.      \y.      \z.   z   v
v3 =          F (\v -> F (\x -> F (\y -> F (\z -> z ! v))))
```

## Encodable ##

Now that we have LC terms equivalent to our Haskell terms, we should make functions to convert between the two. For ease of notation, I'll wrap these functions up in a type class:

```{.haskell pipe="tee -a code.hs"}
class Encodable a where
  encode :: a -> AST
  decode :: AST -> Maybe a

-- Asserts that encoding then decoding a value returns it unchanged
type EncTest a = a -> Bool
enc_dec_test x = decode (encode x) == Just x
```

Encoding is pretty straightforward: pattern-match the Haskell value and spit out the relevant LC value. What about "decode"? We need to reconstruct a value from its encoding; since encoded values are (LC) functions, we can't pattern-match on them; all we can do is apply them to arguments. The trick is to choose arguments which *are* amenable to pattern-matching:

 - "F f" isn't much good, since we can't pattern-match "f".
 - "A x y" is better, since we can pattern-match "x" and "y", but we have to be careful that our terms are in normal form (ie. the applications won't be evaluated).
 - "V x" is excellent, since we can pattern-match "x" and the term itself won't reduce.

This two-level approach of applying encoded values to pattern-matchable LC terms, then pattern-matching those terms from Haskell, nicely handles beta-equivalent terms, but not eta-equivalent terms. Notice that a decoded result is wrapped in "Maybe"; that's because we lose type information when we encode a term. We have no idea whether the AST being passed to "decode" is really an encoded value of the relevant type or not, so we may fail to decode anything.

Let's show how this works with the simplest datatype, the unit type. Since we don't need to do any pattern-matching for the unit type (there's only one possible value), we can use the trivial identity function to represent it:

```{.haskell pipe="tee -a code.hs"}
unit2 :: Unit2
unit2 = id

type Unit2 = forall a. a -> a
```

In LC this gives:

```{.haskell pipe="tee -a code.hs"}
unit3 :: AST
unit3 = F id
```

We can now use this definition to implement the Encodable class:

```{.haskell pipe="tee -a code.hs"}
instance Encodable () where
  encode _ = unit3
  decode _ = Just ()
```

Our "decode" function is easy: we know it should return "()" on success, so we don't even need any error cases. We can verify that it works by using QuickCheck to test our encode/decode assertion, specialised to the unit type:

```
{pipe="ghci"}
:load code.hs
check (enc_dec_test :: EncTest ())
```

Now that we've seen how Encodable works, let's implement a useful type like the booleans. Again, the "encode" function can be built from our existing definitions. The "decode" function needs to pass two distinguishable AST values to the encoded term, then pattern-match to see which one gets returned. We can specify distinguishable ASTs using a set of simple combinators:

```{.haskell pipe="tee -a code.hs"}
-- Mutually-distinguishable ASTs

-- Terminal symbols
trm = V . var

t1, u1, u2, b1 :: AST

t1 = trm 0

-- Unary non-terminals
u1 = F (trm 0 !)
u2 = F (trm 1 !)

-- Binary non-terminal
b1 = F (\x -> F (\y -> (t1 ! x) ! (t1 ! y)))
```

Now we can define our Encodable instance:

```{.haskell pipe="tee -a code.hs"}
instance Encodable Bool where
  encode b = case b of
                  True  -> true3
                  False -> false3
  decode b = case eval (b ! trm 0 ! trm 1) of
                  V Z     -> Just True   -- trm 0
                  V (S Z) -> Just False  -- trm 1
                  _       -> Nothing     -- otherwise
```

```
{pipe="ghci"}
:load code.hs
check (enc_dec_test :: EncTest Bool)
```

Next we implement Encodable for "Maybe a", which is only possible if "a" is Encodable:

```{.haskell pipe="tee -a code.hs"}
instance (Encodable a) => Encodable (Maybe a) where
  encode v = case v of
                  Nothing -> nothing3
                  Just x  -> just3 ! encode x
  decode v = case eval (v ! t1 ! u1) of
                  V Z       -> Just Nothing       -- t1
                  A (V Z) x -> Just <$> decode x  -- u1
                  _         -> Nothing
```

```
{pipe="ghci"}
:load code.hs
check (enc_dec_test :: EncTest (Maybe Bool))
```

```{.haskell pipe="tee -a code.hs"}
instance Encodable Var where
  encode    n  = V n
  decode (V n) = Just n
  decode    _  = Nothing
```

Now we're ready to tackle ASTs themselves. This is a little more complicated, since we have multiple constructors with multiple parameters:

```{.haskell pipe="tee -a code.hs"}
instance Encodable AST where
  encode v = case v of
                  F f   -> f3 ! F f
                  A l r -> a3 ! encode l ! encode r
                  V x   -> v3 ! encode x
  decode v = case eval (v ! u1 ! b1 ! u2) of
                  A (V 0) x                 -> Just x                       -- u1 ! x
                  A (V 1) x                 -> V <$> decode x               -- u2 ! x
                  A (A (V 0) x) (A (V 0) y) -> A <$> decode x <*> decode y  -- b1 ! x ! y
                  _                         -> Nothing
```

```
{pipe="ghci"}
:load code.hs
check (enc_dec_test :: EncTest AST)
```

```
{pipe="sh"}
ghc code.hs
```
