---
title: Self-Improving Code 1, Program-Passing Style
packages: [ 'ghc' ]
---

<!-- Some useful scripts -->

<!-- Append code to our SelfMod Haskell module -->

```{pipe="tee append > /dev/null"}
#!/bin/sh
tee -a SelfMod.hs
echo "" >> SelfMod.hs
```

<!-- Run some Haskell code, after importing the SelfMod module -->

```{pipe="tee run > /dev/null"}
#!/bin/sh
{ echo "import SelfMod"; cat; } | runhaskell
```

```{pipe="tee ghci > /dev/null"}
#!/bin/sh
{ echo ":load SelfMod"; cat; } | ghci -v0 1>&2
```

<!-- Make our scripts executable -->

```{pipe="sh > /dev/null"}
echo "" 1>&2
chmod +x run append ghci
```

```{.haskell pipe="./append > /dev/null"}
module SelfMod where
```

The way we usually solve these kinds of problems is to use our pure functional language to implement a domain-specific language which has the feaures we want.

A simple example of this is mutable state, which we can implement by making it *explicit*: we write functions which take an extra argument containing the state and return an extra value containing any modifications they've made. Once we have this machinery, we can abstract over it to make nice, user-friendly combinator functions.

For example, to log the arguments passed to a function, we could use (log, argument) *pairs*:

```{.haskell pipe="./append | tee lapair.hs"}
data LogPair a = LAP String a
```

```{.haskell pipe="./append"}
getResult :: LogPair a -> a
getResult (LAP _ x) = x

getLog :: LogPair a -> String
getLog (LAP l _) = l

merge :: String -> String -> String
merge l1 l2 = dropWhile (== ' ') (l1 ++ " " ++ l2)

instance Functor LogPair where
  fmap f (LAP l y) = LAP l (f y)

instance Applicative LogPair where
  pure = LAP ""
  (LAP l1 f) <*> (LAP l2 x) = LAP (merge l1 l2) (f x)

instance Monad LogPair where
  return = LAP ""
  (LAP l1 x) >>= f = let (LAP           l2  y) = f x
                      in  LAP (merge l1 l2) y

logArgs :: Show a => (a -> b) -> a -> LogPair b
logArgs f x = LAP (show x) (f x)
```

This lets us write programs which accumulate a log as they go. For example, the following will double a number three times, logging the intermediate results:

```{.haskell pipe="./append"}
double :: Int -> Int
double = (* 2)

logDouble :: Int -> LogPair Int
logDouble = logArgs double

logExample :: LogPair Int
logExample = return 3 >>= logDouble >>= logDouble >>= logDouble
```

This gives the result:

```{pipe="tee l1.hs | ./ghci > l1.out"}
print (getResult logExample)
```

```{.haskell pipe="sh"}
cat l1.hs
```

```{pipe="sh"}
cat l1.out
```

Plus the log:

```{pipe="tee l2.hs | ./ghci > l2.out"}
print (getLog logExample)
```

```{.haskell pipe="sh"}
cat l2.hs
```

```{pipe="sh"}
cat l2.out
```

This is a form of 'writer monad', and we can use the same approach for self-modification, except we pass the *program* around.

## Current-Program as State ##

If we re-use the above pattern for passing *programs* around, we get our first attempt at purely-functional self-modifying code. Let's adapt the logger example to fit our new scenario. Here's what the logger's datastructure looked like:

```{pipe="sh" .haskell}
cat lapair.hs
```

We'll call our new pair `SelfMod1`{.haskell}, so that's what our programs (written in Lambda Calculus) will return:

```{pipe="./append > sm1.hs"}
data SelfMod1 a = SM1 (Program1 a) (Term a)
```

So what is `SelfMod1`{.haskell}? It looks like `LogPair`{.haskell} but we need to replace the `String`{.haskell} (a modified log) with a `Program1`{.haskell} (a modified program):

```{.haskell}
data SelfMod1 a = SM1 (Program1 a) a
```

But what is a `Program1`{.haskell}? It's just a `Term`{.haskell} which outputs a `SelfMod1`{.haskell} pair:

```{.haskell pipe="./append"}
type Program1 a = Term (SelfMod1 a)
```

Notice that the `a` parameter goes through `Program1`{.haskell} and `SelfMod1`{.haskell} unchanged; modifying a program can't change its type, as that would be unsound.

Unfortunately, the second element of the pair can't just be `a`: given a `Term x`, we can't always `eval` it into an `x`. This would prevent us constructing a return value, so instead we'll just keep the `Term a` there, unevaluated:

```{.haskell pipe="sh"}
cat sm1.hs
```

Here's an example of a `Program1` value:

```{.haskell pipe="./append"}
-- A Lambda Calculus Term for halving Ints
halve1 :: Int -> Term Int
halve1 n = let f (C x) = C (x `div` 2)
            in App (Lam f) (Const n)

-- A Program which halves its argument *and* returns a modified version of itself
halveProg1 :: Int -> Int -> SelfMod1 Int
halveProg1 from n = SM1 (halve1 (from+1) n) (halve1 n)
```

This is a function for halving numbers. It also has an internal counter, which doesn't affect the results. Each modification increments the counter in the new program.

Evaluating these is trivial:

```{.haskell pipe="./append"}
smEval1 :: SelfMod1 a -> Partial (Val a)
smEval1 (SM1 _ x) = eval x

getResult1 :: SelfMod1 a -> Partial a
getResult1 x = let f (C n) = n
                   f _     = loop
                   loop    = Later loop
                in smEval1 x >>= f

getProgram1 :: SelfMod1 a -> Program1 a
getProgram1 (SM1 p _) = x
```

```{.haskell piep="./append"}

```

What just happened? We completely ignored the new program! That's because, if we have a `Term a`, we can just evaluate it to get our result; self-modification is only useful for those occasions where we *don't* know how to calculate a result, ie. where we have a `Maybe`:

```{.haskell pipe="./append"}
data SelfMod2 a = SM (Term (SelfMod2 a)) (Maybe (Term a))
```

```{.haskell pipe="./append"}
smEval2 :: SelfMod2 a -> Partial (Val a)
smEval2 (SM _ (Just t)) = eval t
smEval2 (SM p Nothing)  = Later (smEval2 p)
```

This has the expected behaviour: keep modifying until we get a valid `Term`{.haskell}. For example:
The key characteristic of a self-modifying program is that some functions can choose to alter the program, rather than return a result (they might choose to do *both*, which is easy since they have )
The fundamental operation of a self-modifying program is to `switch` to a different codebase. We can represent this with an `Either` value:

```{.haskell}
switch :: (a -> Either b Program) -> a -> Either
```
