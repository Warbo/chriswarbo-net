---
title: "SK logic in egglog: part 2, property-checking extensionality"
packages: [ 'ghcWithFalsify' ]
---

<style>
.pass { background: #00bb0022; }
.fail { background: #bb000022; }
</style>

```{pipe="cat > show && chmod +x show"}
#!/bin/sh
tee -a "$1"
echo >> "$1"
```

```{pipe="cat > hide && chmod +x hide"}
#!/bin/sh
./show "$@" > /dev/null
```

```{pipe="cat > run && chmod +x run"}
#!/bin/sh
set -euo pipefail

# Reads Haskell code on stdin (usually 'main = ...'), returns Pandoc JSON for a
# code block containing it, and another containing the result of executing it.
# The behaviour can be controlled via the following variables:
#  - $* will be spliced after the 'runhaskell' call; e.g. use ' && exit 1' to
#    check for an expected failure.
#  - $NAME: defaults to whatever word follows 'check' or 'checkPred'
#  - $CLASS: defaults to 'pass'; added to both code blocks

cat > in
[[ -n "${NAME:-}" ]] || {
  NAME=$(< in sed -e 's/checkPred/check/g' |
         grep 'check '                |
         sed -e 's/^.*check \([^ ][^ ]*\).*$/\1/g')
}
export NAME
CLASS="${CLASS:-pass}"
rm -f "$NAME"
{
  cat Main.hs
  echo
  cat in
} > "$NAME"
{
  echo "<div class='$CLASS'>"
  echo
  printf '```haskell\n'
  cat in
  printf '\n```\n\n```{pipe="sh"}\n'
  echo "cp root/$NAME ./"
  echo "if runhaskell $NAME 1> out 2> err; then"
  echo "${1:-cat out; cat err 1>&2; exit 0}; else"
  echo "${2:-cat out err 1>&2; exit 1}; fi"
  echo '```'
  echo
  echo '</div>'
} > run.md
if < run.md pandoc -f markdown -t json | panpipe 1> out 2> err
then
  cat err 1>&2
  cat out
  exit 0
else
  {
    echo "BEGIN OUT"
    cat out
    echo "END OUT"
    echo "BEGIN ERR"
    cat err
    echo "END ERR"
    exit 1
  } 1>&2
fi
```

```{pipe="cat > fail && chmod +x fail"}
#!/bin/sh
set -euo pipefail

# Invokes the above 'run' script, but sets $CLASS to 'fail' and expects the
# runhaskell invocation to fail (if it doesn't, it's retried a few times, in
# case falsify failed to find a counterexample we expected)

export CLASS='fail'
# Retry a few times, in case falsify random sampling fails to find a
# counterexample we wanted to show!
GIVEN=$(cat)
for RETRY in seq 1 100
do
  if GOT=$(echo "$GIVEN" | ./run 'cat out err 1>&2 && exit 1' \
                                 'cat out err 2>&1 && exit 0')
  then
    echo "$GOT"
    exit 0
  fi
  done
exit 1
```

This is a spiritual follow-on from [part 1 of my experiments with SK logic in
egglog](/blog/2024-02-25-sk_logic_in_egglog_1.html): that post serves as the
*motivation* for this one, but the content here is somewhat standalone. It's the
result of a rabbit hole I entered, after getting results that were so unexpected
and confusing that it made me question some of the assumptions I'd been making
in those experiments.

I was modelling a small programming language called
[SK (combinatory) logic](https://doisinkidney.com/posts/2020-10-17-ski.html);
and whilst I'm no stranger to being wrong about my code, I had chosen SK since
it's *very* simple and *very* familiar. Implementing SK is like writing
"hello world": something I've done countless times in all sorts of languages and
paradigms, hence why it was an obvious choice when I wanted to learn egglog. In
fact, [combinatory logic even appears in egglog's `examples/`
folder](https://github.com/egraphs-good/egglog/blob/main/tests/combinators.egg)!
However, an innocuous-looking one-line extension brought the whole thing
crashing down: all structure vanished and every term collapsed into one big
blob, where everything equalled everything else.

**Note for non-logicians:** That's
[not good](https://en.wikipedia.org/wiki/Principle_of_explosion)!

Despite several rewrites, trying various different encodings, and much scouring
of [egglog's Rust
source](https://github.com/egraphs-good/egglog/blob/main/src/ast/mod.rs), I kept
running into the same problem. This made me seriously doubt my understanding,
particularly of [free
variables](https://en.wikipedia.org/wiki/Free_variables_and_bound_variables). If
I could spend years immersed in the worlds of mathematics, computer science,
post-graduate academia, and software development; yet fail to grasp such a
fundamental aspect in all that time; then what *else* might I be wrong about? Am
I a complete fraud?

Thankfully I also trained as a physicist, so I know what to do when theoretical
understanding goes awry: **experiment**! Or, as software engineers call it:
**test**! I wanted to thoroughly test the assumptions I was making in those
egglog experiments, so I went crawling back to the comfort of Haskell where I
could use my favourite hammer: [property-based
testing](https://increment.com/testing/in-praise-of-property-based-testing).
This would also let me try out the new
[falsify package](https://hackage.haskell.org/package/falsify) (which
[I've blogged about before](/blog/2024-01-19-lazy_test_generators.html)): in
essence, to try and "falsify myself", to figure out what I'd misunderstood.

This page gives the background of what I was attempting; along with executable
Haskell code, properties it should satisfy, and `falsify` checks of those
properties. The latter are defined using [active code](/projects/activecode).

## The setup ##

I'll explain some of this jargon below, but in short: that problematic line of
code which broke everything was my attempt to implement
[extensional equality](https://en.wikipedia.org/wiki/Extensionality); the idea
that if two things always behave in the same way, then we can consider them
equal. To specify what "always" means, we need some notion of [universal
quantification](https://en.wikipedia.org/wiki/Universal_quantification): that
doesn't quite fit into the formal system of egglog, so my idea was to
*approximate* universally-quantified variables using
[symbolic execution](https://en.wikipedia.org/wiki/Symbolic_execution). This
*should* be a conservative approach: failing to spot some equalities, but never
mistakenly claiming two things to be equal. Instead, it seemed to do the extreme
opposite: *always* claiming that *everything* is equal!

<details class="odd">
<summary>Preamble boilerplate…</summary>

Here's the initial boilerplate for the script; it just needs GHC with `falsify`
in its package database. For more details on how it's executed, see
[this page's Markdown
source](/git/chriswarbo-net/git/branches/master/unfinished/checking_combinators_with_falsify.html).

```{.haskell pipe="./show Main.hs"}
module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (assert)
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Natural (Natural)
import System.Environment (getEnv)
import Test.Falsify.Generator (Gen, Tree(..))
import qualified Test.Falsify.Generator as Gen
import Test.Falsify.Predicate (satisfies)
import Test.Falsify.Property (Property, discard, gen, label, testFailed, testGen)
import Test.Falsify.Range (Range)
import qualified Test.Falsify.Range as Range
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Falsify (testProperty)

-- Useful helpers. These are available in libraries, but I want this code to be
-- self-contained (other than falsify)

-- | Like 'uncurry', but for three arguments
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | Combine two results into a single tuple
tuple2 :: Applicative f => f a -> f b -> f (a, b)
tuple2 a b = (,) <$> a <*> b

-- | Combine three results into a single tuple
tuple3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
tuple3 a b c = (,,) <$> a <*> b <*> c

-- | Combine four results into a single tuple
tuple4 :: Applicative f => f a -> f b -> f c -> f d -> f (a, b, c, d)
tuple4 a b c d = (,,,) <$> a <*> b <*> c <*> d

-- | Like a list, but never ends.
data Stream a = Cons { sHead :: !a, sTail :: Stream a } deriving (Functor)

instance Show a => Show (Stream a) where
  show (Cons x _) = "[" ++ show x ++ ",...]"

sPrefix :: [a] -> Stream a -> Stream a
sPrefix []     ys = ys
sPrefix (x:xs) ys = Cons x (sPrefix xs ys)

-- | Drop elements from a 'Stream' which don't satisfy the given predicate.
sFilter :: (a -> Bool) -> Stream a -> Stream a
sFilter p (Cons x xs) = (if p x then Cons x else id) (sFilter p xs)

-- | The first 'n 'elements of the given 'Stream', as a list.
sTake :: Integral n => n -> Stream a -> [a]
sTake n = fst . sSplitAt n

-- | Split the first 'n' elements off a 'Stream'.
sSplitAt :: Integral n => n -> Stream a -> ([a], Stream a)
sSplitAt = go []
  where go acc n         xs | n <= 0 = (reverse acc, xs)
        go acc n (Cons x xs)         = go (x:acc) (n - 1) xs

-- | Extract the nth element of a 'Stream'.
sAt :: Integral n => n -> Stream a -> a
sAt i (Cons x xs) = if i <= 0 then x else sAt (i - 1) xs

-- | All prefixes of the given Stream, e.g. [], [x], [x, y], [x, y, z], ...
sInits :: Stream a -> Stream [a]
sInits (Cons x xs) = Cons [] ((x:) <$> sInits xs)

-- | All String values, in order of length
allStrings :: Stream String
allStrings = go 0
  where strs 0 = [""]
        strs n = [c:s | c <- [minBound..maxBound], s <- strs (n - 1)]
        go   n = sPrefix (strs n) (go (n + 1))

-- | Like a list, but only stores data at the end. Useful for representing
-- | general recursion.
data Delay a = Now a | Later (Delay a) deriving (Eq, Functor, Ord, Show)

instance Applicative Delay where
  (Now   f) <*> x =        f <$> x
  (Later f) <*> x = Later (f <*> x)
  pure            = Now

instance Monad Delay where
  (Now   x) >>= f = f x
  (Later x) >>= f = Later (x >>= f)

-- | Unwrap (up to) a given number of 'Later' wrappers from a 'Delay' value.
runDelay :: Integral n => n -> Delay a -> Delay a
runDelay _ (Now   x) = Now x
runDelay n (Later x) = (if n <= 0 then id else runDelay (n - 1)) x

-- | Try to extract a value from the given 'Delay', or use the given default
runDelayOr :: Integral n => a -> Delay a -> n -> a
runDelayOr def x n = case runDelay n x of
  Now   x' -> x'
  Later _  -> def
```

</details>

### SK (combinatory logic) ###

Combinatory logic, which I'll shamelessly abbreviate as "SK", is a particularly
simple programming language. It can be written entirely with two symbols,
traditionally called `S` and `K`, along with parentheses for grouping symbols
together. We'll represent SK expressions using the following Haskell datatype
called `Com`{.haskell} (this representation also matches my definition in
egglog):

```{.haskell pipe="./show Main.hs"}
data Com = C String | App Com Com deriving (Eq, Ord)
```

That `deriving`{.haskell} clause asks Haskell to automatically implement some
useful interfaces:

 - `Eq`{.haskell} provides the `==`{.haskell} function. Deriving this lets us
   check whether two `Com`{.haskell} values are identical (same structure and
   same `String`{.haskell} data).
 - `Ord`{.haskell} provides comparisons like `<`{.haskell}. Deriving this gives
   us a
   [lexicographic ordering](https://en.wikipedia.org/wiki/Lexicographic_order),
   but the details aren't important: as long as we have *some* implementation of
   `Ord`{.haskell}, we can use efficient `Set`{.haskell} datastructures.

We'll implement the `Show`{.haskell} interface ourselves, to render expressions
in traditional SK notation:

```{.haskell pipe="./show Main.hs"}
instance Show Com where
  show (C c)             = c
  show (App x (App y z)) = show x ++ "(" ++ show (App y z) ++ ")"
  show (App x y)         = show x ++ show y
```

We represent the "primitives" `S` and `K` using the `C`{.haskell} constructor
(annoyingly, value names in Haskell must begin with a lowercase letter; so we
call these `s`{.haskell} and `k`{.haskell} instead):

```{.haskell pipe="./show Main.hs"}
-- | Primitive SK expressions
s, k :: Com
s = C "S"
k = C "K"
```

`App`{.haskell} groups expressions together two at a time, like a pair of
parentheses. Traditional SK notation only shows parentheses "on the right", e.g.
`App s (App s k)`{.haskell} is written `S(SK)`, but `App (App s k) k`{.haskell}
is simply `SKK`. The following functions extract the left and right children of
an `App`{.haskell} value (returning `Maybe`{.haskell}, in case they're given a
primitive expression):

```{.haskell pipe="./show Main.hs"}
left, right :: Com -> Maybe Com
left  (App x _) = Just x
left  _         = Nothing
right (App _ y) = Just y
right _         = Nothing
```

#### Anatomy of SK expressions ####

We can refer to various parts of an SK expression using the following
terminology:

 - The "root" is the top-most constructor.
 - The "head" is the left-most leaf.
 - The "spine" is the chain of left-nested `App` constructors between the head
   and the root. If the expression is a leaf, its spine is empty.
 - The "arguments" (or "args") of that head are the right-hand children hanging
   off the spine, ordered from the head to the root. If the spine is empty,
   there are no arguments.

Examples can be seen in the figure below. We will often describe an expression
as "applying 〈its head〉 to 〈its arguments〉".

<figure>

```
          𝔸𝕡𝕡
         ╔═╝─┐
        App  S
     ╔═══╝───┐
    App     App
   ╔═╝─┐   ┌─┴─┐
   𝑺   K  App  S
         ┌─┴─┐
         K   K
```

```
    𝔸𝕡𝕡
   ╔═╝─┐
  App  S
 ╔═╝─┐
 𝑲   K
```

<figcaption>Structure of SK expressions: the head is shown in `𝙞𝙩𝙖𝙡𝙞𝙘`, the root
is `𝕕𝕠𝕦𝕓𝕝𝕖-𝕤𝕥𝕣𝕦𝕔𝕜` and the spine is shown with a double═line. **Top:** Structure
of the expression `SK(KKS)S`, represented by the Haskell value
`App (App (App s k) (App (App k k) s)) s`{.haskell}. The head is `𝑺` and
arguments are `K`, `KKS` and `S`. **Bottom:** Structure of that second argument,
`KKS`; with head `𝑲` and arguments `K` and `S`. Leaf expressions (`K` and `S`)
are their own root and head, with no spine or arguments.</figcaption>
</figure>

#### Running SK expressions ####

We can "run" SK expressions using two
[rewriting rules](https://en.wikipedia.org/wiki/Rewriting):

 - An expression applying `K` to two args can be replaced by the first arg.
 - An expression applying `S` to three args can be replaced by an expression
   which applies the first arg to the third arg and «the second applied to the
   third».

We can implement these as Haskell functions, returning `Nothing`{.haskell} if
the rule didn't match:

```{.haskell pipe="./show Main.hs"}
-- | Replaces Kxy with x, otherwise 'Nothing'
stepK :: Com -> Maybe Com
stepK (App (App (C "K") x) _) = Just x
stepK _                       = Nothing

-- | Replaces Sxyz with xz(yz), otherwise 'Nothing'
stepS :: Com -> Maybe Com
stepS (App (App (App (C "S") x) y) z) = Just (App (App x z) (App y z))
stepS _                               = Nothing
```

Notice that arguments can be *any* `Com`{.haskell} value, represented using the
Haskell variables `x`{.haskell}, `y`{.haskell} and `z`{.haskell}; but only
`App`{.haskell}, `s`{.haskell} and `k`{.haskell} have any effect on the output
(this will be important for symbolic execution later!).

Remarkably, these two rules make SK a [universal
(Turing-complete)](https://en.wikipedia.org/wiki/Turing_completeness)
programming language! The following `step` function tries to apply both of these
rules. It also applies the rules recursively to sub-expressions of
`App`{.haskell}: note that we try stepping both children at once, to prevent any
reducible parts of an expression getting "stuck" waiting for their sibling to
finish. `Nothing`{.haskell} is returned when neither rule matched and no
sub-expressions were rewritten; we say the expression is in [normal
form](https://en.wikipedia.org/wiki/Normal_form_(abstract_rewriting)):

```{.haskell pipe="./show Main.hs"}
-- | Attempt to reduce a K or S combinator, or the children of an 'App'.
-- | 'Nothing' if the argument is in normal form.
step :: Com -> Maybe Com
step c = stepK c <|> stepS c <|> app l' r' <|> app l' r <|> app l r'
  where app x y  = App <$> x <*> y
        (l , r ) = (left c    , right c   )
        (l', r') = (l >>= step, r >>= step)
```

Next we'll need to *iterate* the `step`{.haskell} function until its argument
reaches normal form. We'll distinguish normalised expressions using a separate
type called `Normal`{.haskell}:

```{.haskell pipe="./show Main.hs"}
-- | Wraps Com values which are assumed to be in normal form
newtype Normal = N Com deriving (Eq, Ord)

instance Show Normal where
  show = show . toCom

-- | Coerce a 'Normal' back to a 'Com', i.e. forget that it's in normal form.
toCom (N c) = c

-- | 'step' the given 'Com': if it worked, returns that in 'Left'; otherwise
-- | returns the original value (now 'Normal') in 'Right'.
toNormal :: Com -> Either Com Normal
toNormal c = case step c of
  Just c' -> Left     c'
  Nothing -> Right (N c)
```

<details class="odd">
<summary>Understanding `Normal`{.haskell}…</summary>

The `Normal`{.haskell} type has the
[invariant](https://en.wikipedia.org/wiki/Invariant_(mathematics)#Invariants_in_computer_science)
that it only contains `Com`{.haskell} values which are in normal form. More
specifically, we can say `step . toCom`{.haskell} is (extensionally) equal to
`const Nothing`{.haskell}. Values of type `Normal`{.haskell} act as
[evidence](https://cs-syd.eu/posts/2016-07-24-overcoming-boolean-blindness-evidence.html)
that the `Com`{.haskell} they contain is in normal form (i.e. that it will
return `Nothing`{.haskell} when given to `step`{.haskell}).

This invariant is not *guaranteed* to be the case; e.g. we can violate it by
writing code like `N (App (App k s) k)`{.haskell} (since `KSK` reduces to `S`
via the `stepK`{.haskell} rule). To prevent such violations, we avoid using the
`N`{.haskell} constructor directly: instead always using the
`toNormal`{.haskell} function, which only returns a `Normal`{.haskell} value
when it is found to return `Nothing`{.haskell} from `step`{.haskell} (i.e. when
our invariant holds).

`toNormal`{.haskell} is a
[smart constructor](https://kowainik.github.io/posts/haskell-mini-patterns#smart-constructor):
a function which only returns values of the desired type when it's checked
they're valid. Ideally we would *enforce* usage of `toNormal`{.haskell}, by
[encapsulating](https://en.wikipedia.org/wiki/Encapsulation_(computer_programming))
the `N`{.haskell} constructor, making the `Normal`{.haskell} type
[opaque](https://en.wikipedia.org/wiki/Opaque_data_type). That's usually
achieved in Haskell using
[modules](https://en.wikipedia.org/wiki/Modular_programming), but I want the
code for this post to all live in one module (to make compilation and execution
easier).

</details>

`toNormal`{.haskell} tells us when to stop iterating, but since SK is a
universal programming language, there's no way to know beforehand if it ever
will. We'll account for this by wrapping such undecidable computations in
[a `Delay` type](/blog/2014-12-04-Nat_like_types.html#delay-t):

```{.haskell pipe="./show Main.hs"}
-- | Step the given 'Com' until it reaches 'Normal' form.
reduce :: Com -> Delay Normal
reduce c = case toNormal c of
  Left c' -> Later (reduce c')
  Right n -> Now n
```

#### Normal equivalence ####

The rules of SK are
[confluent](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting)),
meaning that every expression has at most one `Normal`{.haskell} form, and
applying the reduction rules in any order, to any parts of an expression, will
not change its `Normal`{.haskell} form or the ability to reach it using those
rules. As a consequence, all SK expressions with the same `Normal`{.haskell}
form are equivalent and interchangable.

To see this, consider two expressions $X$ and $Y$ with the same
`Normal`{.haskell} form $N$. Whenever $X$ occurs inside a larger expression,
confluence allows us to replace $X$ with $N$ without affecting the overall
result. Now imagine we instead replace $X$ with $Y$: whatever that gives us, it
will be unaffected (thanks to confluence) if we reduce $Y$ to its
`Normal`{.haskell} form. Since the `Normal`{.haskell} form of $Y$ is also $N$,
and we already deduced that using $N$ is equivalent to using $X$, the result of
using $Y$ must *also* be equivalent to using $X$. Hence any expressions with the
same `Normal`{.haskell} form are equivalent, in the sense that they can be
interchanged inside any expression without altering its `Normal`{.haskell} form.

We'll call this relationship "`Normal`{.haskell} equivalence". It's
undecidable in general, so it also gets wrapped in `Delay`{.haskell}:

```{.haskell pipe="./show Main.hs"}
-- | Result of comparing two values
data Compared a = Same a | Diff a a deriving (Show)

same, diff :: Compared a -> Bool
same (Same _)   = True
same _          = False
diff (Diff _ _) = True
diff _          = False

-- | Uses (==) to find identical values
comparer :: Eq a => a -> a -> Compared a
comparer x y = if x == y then Same x else Diff x y

-- | Compare the 'Normal' forms of the given 'Com' values.
normalEq :: Com -> Com -> Delay (Compared Normal)
normalEq x y = comparer <$> reduce x <*> reduce y
```

### Property-based testing ###

Let's test this `normalEq`{.haskell} function, to see whether it actually
behaves in the way our theory of SK predicts it should. For example, every
equality relation should have the
["reflexive property"](https://en.wikipedia.org/wiki/Reflexive_relation),
meaning that values should be equal to themselves; so any call like
`normalEq foo foo`{.haskell} should always result in `True`{.haskell}.
Unfortunately, such results are buried in a `Delay`{.haskell} which prevents us
testing them directly. `Delay`{.haskell} values *could* be a never-ending chain
of `Later`{.haskell} wrappers (like `loop = Later loop`{.haskell}), so any
attempt to extract a result from them must eventually give up. We'll work around
this using a parameter that counts down until we give up; this approach is
usually called
[fuel](http://blog.ezyang.com/2011/06/debugging-compilers-with-optimization-fuel):

```{.haskell pipe="./show Main.hs"}
-- | Represents a parameter for "when to give up"
type Fuel = Word
```

Now we can write a predicate (a function returning a boolean) to check whether
its argument is `normalEq`{.haskell} to itself, before its `Fuel`{.haskell} runs
out:

```{.haskell pipe="./show Main.hs"}
normalEqToItself :: (Fuel, Com) -> Bool
normalEqToItself (n, x) = runDelayOr False (same <$> normalEq x x) n
```

We can use this predicate to make statements, by *quantifying* its argument.
Tests commonly use [existential
quantification](https://en.wikipedia.org/wiki/Existential_quantification), which
asserts that *some* argument satisfies `normalEqToItself`{.haskell} (AKA
"example-based" testing). For example:

```{.unwrap pipe="NAME=kIsNormalEqToItself ./run"}
main = assert (normalEqToItself (0, k)) (putStrLn "PASS")
```

However, that isn't really what we want to say: reflexivity doesn't just apply
to a few hand-picked examples, it says that *every argument* satisfies
`normalEqToItself`{.haskell}. Talking about "every argument" is [universal
quantification](https://en.wikipedia.org/wiki/Universal_quantification).
Universally-quantified predicates are called "properties", so this approach is
known as property-based testing.

To test a property, we give it to a "property checker" which searches for
*counterexamples*: argument values which cause the assertion to fail. If no
counterexample can be found, the test passes. Search techniques range from
[simple enumeration](https://hackage.haskell.org/package/smallcheck) all the way
up to [sophisticated AI
algorithms](https://en.wikipedia.org/wiki/American_Fuzzy_Lop_(software)).
Haskell has many property checkers, beginning with the wonderful
[QuickCheck package](https://hackage.haskell.org/package/QuickCheck). We'll use
the state-of-the-art for 2024, which is
[falsify](https://hackage.haskell.org/package/falsify).

#### Data generators ####

`falsify` searches through argument values *at random*, sampling them from a
given "generator" with the Haskell type `Gen a`{.haskell} (for generating values
of some type `a`{.haskell}). We can build up generators using familiar
interfaces (`Applicative`{.haskell}, `Monad`{.haskell}, etc.) from primitives;
such as `Gen.inRange`{.haskell}, which samples fixnums from a `Range`{.haskell}
and is perfect for generating `Fuel`{.haskell}:

```{.haskell pipe="./show Main.hs"}
-- | Generates a (relatively small) amount of Fuel
genFuel = genFuelN limit

-- | Generates up to a certain amount of Fuel
genFuelN :: Fuel -> Gen Fuel
genFuelN = Gen.inRange . to

-- | A reasonable default for procedures requiring Fuel. Small enough to keep
-- | exponentially-growing terms from blowing up.
limit :: Fuel
limit = 20

-- | We usually want Ranges from zero upwards
to :: Fuel -> Range Fuel
to = Range.between . (0,)

-- | A reasonable Range of Fuel to use in tests
small :: Range Fuel
small = to limit
```

Generating `Com`{.haskell} values is more tricky, since they are recursive. A
naïve generator that simply calls itself recursively will make `Com`{.haskell}
values of *exponential* size: either blowing up memory (if the recursive case
`App`{.haskell} is likely to be chosen) or being limited to a handful of tiny
values (if the recursive case is unlikely to be chosen). To generate a diverse
spread of values, without risking out-of-memory conditions, we can use the same
`Fuel`{.haskell} trick as above; this time dividing it up (unevenly) between
recursive calls.

This is such a useful approach that I've implemented it for `QuickCheck`,
`ScalaCheck`, `Hypothesis`, etc. over the years. In contrast, `falsify` provides
it out-of-the-box! The `Gen.tree`{.haskell} function generates binary
`Tree`{.haskell} values of a given size, so we just need to transform those into
the `Com`{.haskell} values we need:

```{.haskell pipe="./show Main.hs"}
-- | Create a Com from a binary Tree (with unit values () at the nodes)
treeToCom :: Tree () -> Com
treeToCom t = case t of
  -- The smallest Trees have one and two leaves. Turn them into K, since that
  -- has the simplest step rule.
  Leaf               -> k
  Branch _ Leaf Leaf -> k

  -- There are two Trees with three leaves. Turn those into S.
  Branch _ Leaf (Branch _ Leaf Leaf) -> s
  Branch _ (Branch _ Leaf Leaf) Leaf -> s

  -- For larger Trees, we recurse on their children and combine using App
  Branch _ l r -> App (treeToCom l) (treeToCom r)

-- | Generate a Com, with size bounded by the given Fuel
genComN :: Fuel -> Gen Com
genComN n = treeToCom <$> Gen.tree (to n) (pure ())

-- | Generate (relatively small) Com values
genCom = genComN limit
```

`falsify` integrates with Haskell's `tasty` test framework. Normally a project
would declare a big test suite to run all at once, but for this literate/active
style we'll be testing things as we go, using the following functions:

```{.haskell pipe="./show Main.hs"}
-- | Turn the given predicate into a falsify property, universally quantified
-- | over the given generator's outputs. Check it on (at least) 100 samples.
checkPred :: Show a => (a -> Bool) -> Gen a -> IO ()
checkPred pred gen = do
  name <- testName
  check (testProperty name (testGen (satisfies (name, pred)) gen))

-- | Check the given falsify 'Property' holds for (at least) 100 samples. Prints
-- | a counterexample if found; or else some statistics about the search.
check = defaultMain

-- | We'll put test names in an env var to avoid repetition
testName :: IO String
testName = getEnv "NAME"
```

#### Included middles ####

The observant among you may have noticed that `normalEqToItself`{.haskell}
**does not** hold for all argument values! `falsify` can generate a
counterexample to show us why:

```{.unwrap pipe="./fail"}
main = checkPred normalEqToItself (tuple2 genFuel genCom)
```

The precise counterexample `falsify` finds may vary depending on the random
seed, but they'll all have the following in common: the `Fuel`{.haskell} will be
`0`{.haskell}, whilst the `Com` expression will not be in normal form. For
example, it may produce `KKK` (the Haskell value `App (App k k) k`{.haskell}).
`stepK`{.haskell} will reduce that to a single `K`, so whilst the value
`KKK` *is* equal to itself (as we expect), `normalEq`{.haskell} wraps it in a
`Later`{.haskell} constructor, which may cause `normalEqToItself`{.haskell} to
give up for *some* `Fuel`{.haskell} parameters: in particular, when given
`0`{.haskell} `Fuel`{.haskell}. This disproves our claim that the property holds
for *any* amount of `Fuel`{.haskell}.

We could alter our claim to *existentially* quantify the amount of
`Fuel`{.haskell}: that for any SK expression `x`{.haskell}, there is *some*
value of `n`{.haskell} where
`runDelayOr False (same <$> normalEq x x) n`{.haskell} holds. However
that is *also* false, since there are infinite loops which have no
`Normal`{.haskell} form; hence never produce a `Now` value; and therefore cannot
have a result extracted regardless of how much `Fuel`{.haskell} we use. Even
when a `Normal`{.haskell} form *does* exist, there is no way to bound
[the amount of `Fuel`{.haskell} required to find
it](https://en.wikipedia.org/wiki/Busy_beaver).

The correct way to fix our claim is to universally quantify `x`{.haskell} and
`n`{.haskell} as before, but add two negations to our predicate: instead of
claiming every expression is equal to itself (regardless of `Fuel`{.haskell}),
we claim that every expression is *not unequal* to itself (regardless of
`Fuel`{.haskell}):

```{.haskell pipe="./show Main.hs"}
notUnnormalEqToItself :: (Fuel, Com) -> Bool
notUnnormalEqToItself (n, x) = runDelayOr True (not . diff <$> normalEq x x) n
```

```{.unwrap pipe="./run"}
main = checkPred notUnnormalEqToItself (tuple2 genFuel genCom)
```

You may have learned in school that [double-negatives are
redundant](https://en.wikipedia.org/wiki/Double_negation_elimination): that
anything "not false" must be "true" (the
[law of the excluded middle](https://en.wikipedia.org/wiki/Law_of_excluded_middle)).
However, we're not dealing with the true/false ideals of
[classical logic](https://en.wikipedia.org/wiki/Classical_logic), which ignore
computation and its associated undecidability. All we have are the more
pragmatic falsified/unfalsified results of a property checker, where there are
non-excluded middles such as "don't know", "timed out" and "gave up"!

### Extensional equality ###

My problems arose when trying to extend the equality of SK expressions even
further to include
[extensional equality](https://en.wikipedia.org/wiki/Extensionality). To explain
how this works, we'll need a bit more terminology…

#### Inputs and agreement ####

To understand the behaviour of a particular SK expression, we can see what
happens when it's applied to other SK expressions. We'll call the latter "input
values", to emphasise that they are not part of the original expression we're
focused on. For example, we can understand how the expression `KS` behaves by
applying it to an input value like `S`: giving `KSS` (which reduces to
`Normal`{.haskell} form `S`). Note that `KS` contains head `K` and argument `S`;
whilst the resulting expression `KSS` also contains the input value `S` as an
extra argument.

If we apply two expressions to the same input value, and the results reduce to
the same `Normal`{.haskell} form, we say those two expressions "agree on" that
input value. For example, `KK` and `SKK` agree on the input value `K`, since
`KKK` reduces to `K`; and `SKKK` reduces to `KK(KK)` then to `K`. Note that they
*disagree* on the input value `S`, since `KKS` reduces to `K`; whilst `SKKS`
reduces to `KS(KS)` then to `S`. Expressions can also agree on a *sequence* of
input values, where we apply them both to the first value, then apply those
results to the second value, and so on. We'll represent such sequences using a
`Stream`{.haskell}, which is like a list except there is no "nil" case, so it
goes on forever:

```{.haskell pipe="./show Main.hs"}
type InputValue  = Com
type InputValues = Stream InputValue

-- | Apply the given 'Com' expressions to more and more of the 'InputValues',
-- | checking whether they reach the same 'Normal' form.
agree :: Com -> Com -> InputValues -> Stream (Delay (Compared Normal))
agree f g (Cons x xs) = Cons (normalEq f g) (agree (App f x) (App g x) xs)
```

Everything that satisfies `normalEq`{.haskell} should also satisfy
`agree`{.haskell}, which we can state with the following property:

```{.haskell pipe="./show Main.hs"}
normalEqImpliesAgree :: (Fuel, Com, Com, InputValues) -> Bool
normalEqImpliesAgree (n, f, g, xs) =
  case runDelay n (tuple2 (normalEq f g) (sHead (agree f g xs))) of
    Now (Same _, Diff _ _) -> False
    _                      -> True

-- | Generate a 'Stream' of Com values, with element size bounded by 'Fuel'
genComsN :: Fuel -> Gen InputValues
genComsN fuel = Cons <$> genComN fuel <*> genComsN fuel

-- | Generate (relatively small) lists of Com values
genComs :: Gen InputValues
genComs = genComsN limit
```

```{.unwrap pipe="./run"}
main = checkPred normalEqImpliesAgree (tuple4 genFuel genCom genCom genComs)
```

We say expressions "agree on $N$ inputs" when they agree on *every* sequence of
$N$ input values; i.e. the input values are universally-quantified, but the
length of the sequence is not. For example, `SK` and `S(K(SK))(KK)` agree on two
inputs; which we can test by asserting that they *never disagree*:

```{.haskell pipe="./show Main.hs"}
skNeverDisagreesWithSKSKKK :: (Fuel, InputValues) -> Bool
skNeverDisagreesWithSKSKKK (n, xs) =
    runDelayOr True (not . diff <$> sAt (2 + n) (agree f g xs)) n
  where f = App s k
        g = App (App s (App k (App s k))) (App k k)
```

```{.unwrap pipe="./run"}
main = checkPred skNeverDisagreesWithSKSKKK (tuple2 genFuel genComs)
```

Note that any expressions which agree on $N$ inputs also agree on $N+1$ inputs
(and so on), since the left child of each root has the same `Normal`{.haskell}
form (by definition of agreement on $N$ inputs).

```{.haskell pipe="./show Main.hs"}
agreementIsMonotonic :: ((Fuel, Fuel), (Com, Com), InputValues) -> Bool
agreementIsMonotonic ((n, m), (f, g), xs) =
  case runDelay n (tuple2 (sAt  n      (agree f g xs))
                          (sAt (n + m) (agree f g xs))) of
    Now (Same _, Diff _ _) -> False
    _                      -> True
```

```{.unwrap pipe="./run"}
main = checkPred agreementIsMonotonic (tuple3 (tuple2 genFuel genFuel)
                                              (tuple2 genCom  genCom )
                                              genComs)
```

#### Extensionality ####

Now we've defined inputs and agreement, extensional equality becomes quite
simple: SK expressions are extensionally equal iff they agree on *some* number
of inputs. We saw that `SK` and `S(K(SK))(KK)` agree on *some* number of inputs
(namely: on two inputs), so they are extensionally equal. Expressions which are
normally equivalent are also extensionally equal, since they agree on zero
inputs.

Notice that the *number* of inputs is existentially-quantified, whilst the
*values* of those inputs are universally-quantified. This makes extensional
equality difficult to determine:

 - If we compare two expressions on $N$ input values and see that they *agree*,
   that doesn't mean they're extensionally equal; since there might be other
   input values of length $N$ for which they disagree.
 - If we compare two expressions on $N$ input values and see that they
   *disagree*, that doesn't mean they're *not* extensionally equal; since they
   might only agree on inputs longer than $N$.

In order to make more definitive assertions about extensional equality, we need
to borrow techniques from the world of formal methods!

### Symbolic execution ###

The reason I like property-checking is that it can provide a lot of confidence
for relatively little effort, compared to e.g. example-based testing (less
confidence for a similar effort), manual testing (less confidence for more
effort) or formal verification (more confidence for more effort). However, there
are occasions when the scales tip in favour of other approaches, and extensional
equality checking turns out to be particularly suited to a formal method called
[symbolic execution](https://en.wikipedia.org/wiki/Symbolic_execution).

In this approach, we avoid using real, "concrete" SK expressions for our input
values; and instead use
[abstract symbols](https://en.wikipedia.org/wiki/Symbol_(formal)). These can be
copied, discarded or rearranged during program execution; which is perfect for
seeing where different inputs end up in the resulting `Normal`{.haskell} form.
They are otherwise inert or "uninterpreted": causing reduction to stop if it
would depend on the particular value of an input.

Implementing symbolic execution can often be laborious, but our definition of
`Com`{.haskell} actually makes it trivial to graft on top: we can represent
abstract symbols using the `C`{.haskell} constructor applied to any
`String`{.haskell} (*except* `"S"`{.haskell} or `"K"`{.haskell}, which we're
treating specially). Execution of the `stepK`{.haskell} and `stepS`{.haskell}
functions will treat these as
[atomic symbols](https://en.wikipedia.org/wiki/Symbol_(programming)), to be
copied, discarded and rearranged as needed.

Once we've normalised a `Com`{.haskell} value containing symbols, we need a
semantics, or [abstract
interpretation](https://en.wikipedia.org/wiki/Abstract_interpretation), to
understand what they mean. Since we're using each symbol to represent a
distinct input, the way they appear in `Normal`{.haskell} forms tells us
something about the overall behaviour of the expression and, crucially, how it
depends on the particular *concrete* `Inputs`{.haskell} it may be applied to.
It's undecidable whether part of an SK expression will influence its reduction
(or just get discarded by the `stepK`{.haskell} rule), so we'll limit our
ambitions to understanding a few simple situations.

#### Symbolic agreement proves extensional equality ####

Expressions which agree on symbolic inputs will agree on *all* inputs, since
symbols do not reduce, and hence those inputs must have been irrelevant for the
reductions which lead to the agreement. Unlike property-checking, where we rely
on double-negatives to "fail to disprove agreement", this is real positive proof
that expressions will *always* agree, and hence we can claim definitively that
they *are* extensionally equal.

We don't need to implement this check specially, since we can reuse
`agree`{.haskell}, just using symbolic values as our inputs instead of concrete
SK expressions:

```{.haskell pipe="./show Main.hs"}
-- | Synonym to indicate when we're dealing with uninterpreted symbolic values
type Symbol = String

-- | All Symbols except "S", "K" and "" (which doesn't show well)
symbols :: Stream Symbol
symbols = sFilter keep allStrings
  where keep s = s /= "" && s /= "S" && s /= "K"
```

The following `agreeSym`{.haskell} function applies two expressions to more and
more symbolic inputs, to see if they reduce to the same `Normal`{.haskell} form:

```{.haskell pipe="./show Main.hs"}
-- | Checks whether two Com values agree on more and more symbolic input values.
agreeSym :: Com -> Com -> Stream (Delay (Compared Normal))
agreeSym f g = agree f g (C <$> symbols)
```

The return type of `agreeSym`{.haskell} is a little awkward, since it's "two
dimensional": travelling further along the `Stream`{.haskell} applies more and
more symbolic inputs; travelling further along any of those `Delay`{.haskell}
values applies more and more steps to that expression. To get at the results
inside (assuming any of those expressions has a `Normal`{.haskell} form) we need
a linear path which traverses this structure. We can't use breadth-first search
since the `Stream`{.haskell} never ends; we also can't use depth-first search,
since a `Delay`{.haskell} might never end. The following `race`{.haskell}
function is a bit smarter: it acts like a round-robin scheduler, each iteration
running more expressions until one of them finishes (if ever):

```{.haskell pipe="./show Main.hs"}
-- | Runs every 'Delay' element in an interleaved fashion until one of them
-- | produces a 'Now'. Returns that result, its index in the 'Stream', and a
-- | 'Stream' of the remaining 'Delay' elements.
race :: Stream (Delay a) -> Delay (a, Natural, Stream (Delay a))
race s = go 1 ([], s)
  where go !n (  Now x:xs, ys) =   Now (x,  n-1, sPrefix xs               ys)
        go !n (Later x:xs, ys) = Later (go (n+1) (xs, Cons (runDelay 1 x) ys))
        go !n ([]        , ys) =        go    1  (sSplitAt n              ys )
```

We can use `race`{.haskell} to check whether two expressions ever agree on
symbolic inputs, and therefore whether they're extensionally equal:

```{.haskell pipe="./show Main.hs"}
-- | 'True' iff the given expressions ever agree for any number of inputs.
everAgree :: Com -> Com -> Delay Bool
everAgree x y = race (agreeSym x y) >>= go
  where go (Same _  , _, _) = Now True
        go (Diff _ _, _, s) = race s >>= go
```

Note that `everAgree`{.haskell} is more general than `normalEq`{.haskell}, since
the latter only checks for agreement on $0$ inputs:

```{.haskell pipe="./show Main.hs"}
normalEqImpliesEverAgree :: (Fuel, Com, Com) -> Bool
normalEqImpliesEverAgree (n, x, y) = if      go (same <$> normalEq x y)
                                        then go (        everAgree x y)
                                        else True
  where go d = runDelayOr False d n
```

```{.unwrap pipe="./run"}
main = checkPred normalEqImpliesEverAgree (tuple3 genFuel genCom genCom)
```

However, `everAgree`{.haskell} is not yet a predicate for checking extensional
equality, since the `Compared`{.haskell} values returned by `agreeSym`{.haskell}
don't represent "equal/unequal"; only "equal/unsure". Hence the
`everAgree`{.haskell} function *claims* to return `Delay Bool`{.haskell}, which
we can think of as "at most one boolean". Yet its result is not really a
boolean, since it can never be `False`{.haskell}! It would be more accurate to
return a unit value `Now ()`{.haskell}, of type `Delay ()`{.haskell}. That type
is [isomorphic to the natural numbers](/blog/2014-12-04-Nat_like_types.html),
counting how long it took to prove equality. That can't be a predicate, since it
[begs the question](https://en.wikipedia.org/wiki/Begging_the_question)!

A *useful* predicate for extensional equality not only needs to return
`True`{.haskell} for some inputs it's sure are equal; but *also* return
`False`{.haskell} for some inputs it's sure are unequal. When it's unsure (which
is unavoidable due to undecidability), it can use a never-ending chain of
`Later`{.haskell} wrappers to avoid ever returning at all!

#### Different symbolic heads prove disagreement ####

We can easily show that two expressions disagree for $N$ inputs, by checking if
the $N$th element of `agreeSym`{.haskell} results in `Diff`{.haskell}. To be
*sure* they are not extensionally equal, we also need to prove they will
disagree for all inputs *longer* than $N$.

A simple way to prove this is when the head of each expression is a different
symbolic input. In that case, their `Normal`{.haskell} forms depend entirely on
those inputs, so disagreement can be "forced" by choosing input values which
reduce to different results. For example, say `Xab` is some concrete SK
expression applied to two symbolic inputs, which happens to reduce to `aS`;
hence its result depends on the *first* input, and is independent of the second.
If some other expression `Yab` reduces those inputs to `bS`, it depends on the
*second* input, and is independent of the first. They can be forced to disagree
by choosing concrete input values in place of `a` and `b`, which reduce to
values that are known to be unequal. In this example we could use input values
`KS` and `KK`, which `X` reduces to `S` but `Y` reduces to `K`. This works even
if both inputs appear in a result, e.g. `ab` can still be forced independently
of `b`, by choosing a concrete expression for `a` which discards its argument.

This ability to "force" the result of expressions which use an input as their
head can be used to extend disagreements from $N$ inputs to *all* inputs
$M > N$. To see this, note that the result of applying an expression to $M > N$
inputs is equivalent to first applying it to $N$ inputs (which we can force to
result in any value we like), then applying that result to the remaining $M - N$
inputs. We can choose those intermediate results to be expressions that consume
the remaining $M - N$ inputs (e.g. through repeated use of `K`) *and then*
disagree with each other. ∎

The following function will spot when two expressions have different symbols in
head position, and are hence provably unable to agree, even with more inputs:

```{.haskell pipe="./show Main.hs"}
-- | Return the head (left-most leaf) of the given Com
headPos :: Com -> Symbol
headPos (C c)     = c
headPos (App l r) = headPos l

-- | Whether a String represents an uninterpreted Symbol (i.e. not S or K)
isSym :: String -> Bool
isSym s = s /= "S" && s /= "K"

-- | Whether the given Com values have distinct symbolic values in head position
distinctSymbolicHeads :: Com -> Com -> Bool
distinctSymbolicHeads x y = isSym hX && isSym hY && hX /= hY
  where (hX, hY) = (headPos x, headPos y)
```

We can perform a few sanity checks, to confirm that our argument above holds:

```{.haskell pipe="./show Main.hs"}
-- | Generate String values to represent uninterpreted symbolic values
genSymN :: Fuel -> Gen Symbol
genSymN n = (`sAt` symbols) <$> genFuelN n

-- | Generate (relatively small) symbolic values
genSym :: Gen Symbol
genSym = genSymN limit

-- | Generate Com values which may also contain symbols
genSymComN :: Fuel -> Gen Com
genSymComN n = toSymCom <$> Gen.tree (to n) (genSymN n)
  where toSymCom t = case t of
          Leaf                               -> k
          Branch _ Leaf Leaf                 -> k
          Branch _ Leaf (Branch _ Leaf Leaf) -> s
          Branch s (Branch _ Leaf Leaf) Leaf -> C s
          Branch _ l r                       -> App (toSymCom l) (toSymCom r)

-- | Generate (relatively small) Com values which may contain symbols
genSymCom = genSymComN limit

-- | Shouldn't matter which order we compare two Com values
distinctSymbolicHeadsCommutes :: (Com, Com) -> Bool
distinctSymbolicHeadsCommutes (x, y) = distinctSymbolicHeads x y
                                    == distinctSymbolicHeads y x
```

```{.unwrap pipe="./run"}
main = checkPred distinctSymbolicHeadsCommutes (tuple2 genSymCom genSymCom)
```

#### Different numbers of arguments prove disagreement ####

Expressions whose heads are *the same* symbolic input, but applied to a
different number of arguments, can also be forced to disagree. For example, if
`Xab` reduces to `aSb`, and `Yab` reduces to `aS`, then
`distinctSymbolicHeads`{.haskell} can't be sure if they're different. Yet we can
still force them to disagree, by giving them *three* inputs (`a`, `b` and `c`):

 - We choose a value for `a` which consumes two arguments and reduces to the
   second, such as `SK`.
 - We give `b` the value `Kd` (where `d` can represent any expression).
 - We leave `c` unconstrained.

`X` and `Y` will reduce these inputs as follows:

```
X(SK)(Kd)       c    |    Y(SK)(Kd)c        | Xabc and Yabc
  SKS(Kd)       c    |      SKS    c        | Reduced per example
   K (Kd)(S(Kd))c    |       K     c(Sc)    | Applied S rule to each
      Kd        c    |             c        | Applied K rule to each
       d             |             c        | Applied K rule again to first
```

Since `X` and `Y` reduce to different symbols, we can use this scheme to force
their results to *any* value, including unequal `Normal`{.haskell} forms or
(as above) expressions which consume any number of subsequent inputs *then*
become unequal `Normal`{.haskell} forms. ∎

The following `unequalArgCount`{.haskell} function will spot when two
expressions apply a symbol to an unequal number of arguments, and are hence sure
to be extensionally unequal:

```{.haskell pipe="./show Main.hs"}
-- | Split a Com value into its head and any arguments that's applied to
headAndArgs :: Com -> (String, [Com])
headAndArgs (C x) = (x, [])
headAndArgs (App l r) = case headAndArgs l of
  (h, args) -> (h, args ++ [r])

-- | True iff the given expressions have the same Symbol in their head, but
-- | applied to a different number of arguments.
unequalArgCount :: Com -> Com -> Bool
unequalArgCount x y = isSym headX
                   && headX == headY
                   && length argsX /= length argsY
  where (headX, argsX) = headAndArgs x
        (headY, argsY) = headAndArgs y
```

Again, we can sanity-check this:

```{.haskell pipe="./show Main.hs"}
-- | Order of Com values shouldn't affect result
unequalArgCountCommutes :: (Com, Com) -> Bool
unequalArgCountCommutes (x, y) = unequalArgCount x y
                              == unequalArgCount y x
```

```{.unwrap pipe="./run"}
main = checkPred distinctSymbolicHeadsCommutes (tuple2 genSymCom genSymCom)
```

#### Disagreeing arguments prove disagreement ####

Finally, we can force expressions to disagree by forcing a disagreement *inside*
them, then propagating it up to the overall result. This is useful, since it can
be achieved when each expression has the *same* `Symbol` at their heads
(avoiding `distinctSymbolicHeads`{.haskell}), and they apply it to the *same*
number of arguments (avoiding `unequalArgCount`{.haskell}); as long as we can
force the *values* of those arguments to disagree.

For example, let's say `Xabc` reduces to `ab` and `Yabc` reduces to `ac`: these
have the same symbol `a` as their heads, and both apply it to a single argument.
However, the values of those arguments (`b` and `c`) are provably unequal (via
`distinctSymbolicHeads`{.haskell}, in this case). We can hence use the first
input `a` to propagate its unequal argument, e.g. using the input value `SKK`.
Since `X(SKK)bc` reduces to `b`, and `Y(SKK)bc` reduces to `c`, we can force a
disagreement by choosing unequal values for `b` and `c`; and this extends to
more inputs by having them consume the remainder, as before. ∎

The following function checks for the situation described above. It checks
whether argument values are unequal using a given `unEq`{.haskell} parameter,
which is more general and useful than if we hard-coded some specific check:

```{.haskell pipe="./show Main.hs"}
-- | Whether the given Com values have matching symbols in their heads, but
-- | applied to unequal values (determined by the given unEq function)
symbolGivenUnequalArgs :: (Com -> Com -> Bool) -> Com -> Com -> Bool
symbolGivenUnequalArgs unEq x y = isSym headX
                               && headX == headY
                               && any id (zipWith unEq' argsX argsY)
  where (headX, argsX) = headAndArgs x
        (headY, argsY) = headAndArgs y
        unEq' a b      = unEq a b || unEq b a  -- Compare both ways round

-- | The order of arguments shouldn't alter the result
symbolGivenUnequalArgsCommutes :: (Com -> Com -> Bool) -> Com -> Com -> Bool
symbolGivenUnequalArgsCommutes f x y = symbolGivenUnequalArgs f x y
                                    == symbolGivenUnequalArgs f y x
```

<details class="odd">
<summary>Generating `unEq`{.haskell} functions to test with…</summary>

The `symbolGivenUnequalArgs`{.haskell} function doesn't care *how* arguments are
deemed to be unequal, so it lets the caller decide by passing in a function. The
choice of function shouldn't affect commutativity, so the property
`symbolGivenUnequalArgsCommutes`{.haskell} is universally quantified over that
choice; allowing a counterexample search to try many different functions.

This may seem surprising if you've not encountered it before, but we can
generate a function value just as easily as a "data-like" value. For example, we
can generate functions which look up their arguments in a (generated) lookup
table:

```{.haskell pipe="./show Main.hs"}
genViaTable :: Eq a => Range Fuel -> Gen a -> Gen b -> Gen (a -> b)
genViaTable range genA genB = do
  def   <- genB
  pairs <- Gen.list range (tuple2 genA genB)
  pure (maybe def id . (`lookup` pairs))
```

However, such ordinary function values are opaque "black boxes", which makes
them less useful for property-checking: firstly, it's hard to `show`{.haskell}
them (e.g. if they form part of a counterexample); and secondly it's hard to
shrink them (to look for "simpler" alternatives). Property-checkers avoid these
problems by replacing ordinary functions `a -> b`{.haskell} with their own
alternative, whose constructors 'remember' how to `show`{.haskell} and shrink
themselves.  In `falsify` this alternative is `Gen.Fun a b`{.haskell}, so we'll
need to convert the `Com -> Com -> Bool` argument of
`symbolGivenUnequalArgsCommutes`{.haskell} into either
`Fun Com (Fun Com Bool)`{.haskell}
([curried](https://en.wikipedia.org/wiki/Currying) form) or, equivalently,
`Fun (Com, Com) Bool`{.haskell} (uncurried form). We'll use the uncurried form,
since the tuples will be handled automatically by type class instance
resolution. The following helpers let `symbolGivenUnequalArgsCommutes`{.haskell}
take a `Fun` as argument:

```{.haskell pipe="./show Main.hs"}
-- | Transform a higher-order function to take a Fun instead
liftFun :: ((a -> b) -> c) -> Gen.Fun a b -> c
liftFun f = f . Gen.applyFun

-- | Lift a function with binary argument to take an uncurried Fun instead
liftFun2 :: ((a -> b -> c) -> d) -> Gen.Fun (a, b) c -> d
liftFun2 f = liftFun (f . curry)
```

To check (the lifted version of) `symbolGivenUnequalArgsCommutes`{.haskell}, we
need a generator of type `Gen (Fun (Com, Com) Bool)`{.haskell}. We can get one
from `Gen.fun`{.haskell}, but that requires an instance of
`Gen.Function (Com, Com)`{.haskell}. There's an existing instance for tuples,
but we still need to implement `Gen.Function Com`{.haskell} ourselves:

```{.haskell pipe="./show Main.hs"}
instance Gen.Function Com where
  function gen = Gen.functionMap comToPre preToCom <$> Gen.function gen
```

This is piggybacking on an existing instance (the `Gen.function gen`{.haskell}
call at the end) by converting our `Com`{.haskell} values back-and-forth to
another type that already implements `Gen.Function`{.haskell}. The type I've
chosen is `[Maybe (Either Bool Symbol)]`{.haskell}, and the conversions are
implemented by `comToPre`{.haskell} & `preToCom`{.haskell}:

```{.haskell pipe="./show Main.hs"}
-- | Prefix notation for expressions. `Nothing` represents an 'apply' operation.
type Prefix = [Maybe (Either Bool Symbol)]

-- | Converts expressions from "applicative style" Com to "prefix style" Prefix.
comToPre :: Com -> Prefix
comToPre x = case x of
  -- Treat K and S specially, so they're more likely to be generated
  _ | x == k -> [Just (Left False)]
  _ | x == s -> [Just (Left True )]
  -- Any other symbol
  C c        -> [Just (Right c)]
  -- Nothing acts like an apply operator for two expressions that follow it
  App l r    -> [Nothing] ++ comToPre l ++ comToPre r

-- | Parses "prefix style" Prefix back into an "applicative style" Com.
preToCom :: Prefix -> Com
preToCom = fst . go          -- Discard any remaining suffix
  where go []     = (k, [])  -- Makes preToCom total, but overlaps 'Left False'
        go (x:xs) = case x of
          Just (Left False) -> (k  , xs)
          Just (Left True ) -> (s  , xs)
          Just (Right c   ) -> (C c, xs)
          Nothing           -> let (l, ys) = go xs
                                   (r, zs) = go ys
                                in (App l r, zs)

comToPreRoundtrips :: Com -> Bool
comToPreRoundtrips c = preToCom (comToPre c) == c
```

```{.unwrap pipe="./run"}
main = checkPred comToPreRoundtrips genCom
```

```{.haskell pipe="./show Main.hs"}
preToComAlmostRoundtrips :: Prefix -> Bool
preToComAlmostRoundtrips p = comToPre (preToCom p') == p'
  where p' = comToPre (preToCom p)  -- Extra roundtrip to make p "correct"

genPre :: Gen Prefix
genPre = Gen.list small genEntry
  where genEntry = Gen.choose (Just <$> genLeaf) (pure Nothing)
        genLeaf  = Gen.choose (Left <$> Gen.bool False) (Right <$> genSym)
```

```{.unwrap pipe="./run"}
main = checkPred preToComAlmostRoundtrips genPre
```

The `Prefix`{.haskell} type is a
[prefix form](https://en.wikipedia.org/wiki/Polish_notation) for expressions, as
opposed to the "applicative form" of `Com`{.haskell}. They're equivalent, but
the structure of an expression is less obvious in prefix form, which is why it
only appears in this hidden section. This is the encoding used in [binary
combinatory logic](https://en.wikipedia.org/wiki/Binary_combinatory_logic),
although we're allowing arbitrary `Symbol`{.haskell} values rather than just `S`
and `K`.

The functions `preToCom`{.haskell} and `comToPre`{.haskell} don't *quite* form
an [isomorphism](https://en.wikipedia.org/wiki/Isomorphism). This is essentially
due to `Prefix`{.haskell} being a [prefix(-free)
code](https://en.wikipedia.org/wiki/Prefix_code):

 - The empty list `[]`{.haskell} does not correspond to a particular
   `Com`{.haskell} value. It a valid *prefix*, since appending it with anything
   will either form a complete expression or another valid prefix; but that's
   not so useful for `preToCom`{.haskell}, which might have only been given a
   *finite* list and needs to return a `Com`{.haskell}. In this case, it just
   returns `K` (arbitrarily).
 - `Prefix`{.haskell} values may contain "too many" `Nothing`{.haskell} values.
   These act like `App`{.haskell}, indicating that the following expression is
   applied to the one after. That "following expression" may itself use
   `Nothing`{.haskell} to consume subsequent list elements, and so on;
   corresponding to the arbitrary nesting which `App`{.haskell} allows. However,
   the given `Prefix`{.haskell} may "run out" before specifying what those
   following elements should be! Again, that's a perfectly good *prefix*, but
   not a complete value. When this happens, we will hit the same `[]`{.haskell}
   case as above, and simply return `K` (potentially many times, as the
   applications get "popped off the stack").
 - Expressions encoded as `Prefix`{.haskell} values are self-delimiting, meaning
   that the encoding itself tells us when to finish parsing: if the value we
   parsed was `Just _`{.haskell}, we have finished that expression; if the value
   is `Nothing`{.haskell} we can finish after parsing exactly two more
   expressions. Hence any elements appearing *after* a correctly-encoded
   expression will be completely ignored, resulting in the same `Com`{.haskell}.
 - Our hacky use of `K` as a fallback for empty `Prefix`{.haskell} values
   introduces some ambiguity, since an expression like `KK` could be encoded as
   `[Nothing, Just (Left False), Just (Left False)]`{.haskell} (the "correct"
   way); or as `[Nothing, Just (Left False)]`{.haskell} (relying on the fallback
   behaviour to introduce another `K`); or indeed as `[Nothing]`{.haskell}
   (relying on the fallback for *both* `K` expressions). Hence the fallback
   behaviour should not be relied upon; also, it breaks when composing
   expressions together (since missing `K` values will only be introduced at the
   end of a list).
 - There's also some redundancy between
   `Left False`{.haskell}/`Left True`{.haskell} and
   `Right "K"`{.haskell}/`Right "S"`{.haskell}. This was a deliberate choice, to
   make `falsify` generate more `S` and `K` expressions.

These aren't a problem for our use of `Prefix`{.haskell} in generating values;
but you may need to keep them in mind when using this encoding for other
purposes (in which case I'd recommend *embracing* its nature as a prefix(-free)
code, since it has all sorts of cool applications!)

</details>

```{.unwrap pipe="NAME=symbolGivenUnequalArgsCommutes ./run"}
main = checkPred (uncurry3 (liftFun2 symbolGivenUnequalArgsCommutes))
                 (tuple3 (Gen.fun (Gen.bool False)) genSymCom genSymCom)
```

#### Combining disagreement provers ####

We can never spot *all* disagreements, but the simple checks above can be
combined into a single, reasonably-useful function. Not only is this easier to
use than the individual checks, but we can also use it as the `unEq`{.haskell}
argument of the `symbolGivenUnequalArgs`{.haskell} function, which lets us
recurse through the given expressions looking for disagreement at any depth!

```{.haskell pipe="./show Main.hs"}
provablyDisagree :: Com -> Com -> Bool
provablyDisagree x y = distinctSymbolicHeads                   x y
                    || unequalArgCount                         x y
                    || symbolGivenUnequalArgs provablyDisagree x y

provablyDisagreeCommutes :: (Com, Com) -> Bool
provablyDisagreeCommutes (x, y) = provablyDisagree x y == provablyDisagree y x
```

```{.unwrap pipe="./run"}
main = checkPred provablyDisagreeCommutes (tuple2 genSymCom genSymCom)
```

#### Approximating extensional equality ####

Now we have ways to spot both extensional equality *and* inequality (in certain
situations), we can fold them over the result of `agreeSym`{.haskell}. We'll
begin by extracting the numerical index provided by `race`{.haskell}, which
should tell us how many inputs it takes for the given expressions to start
agreeing:

```{.haskell pipe="./show Main.hs"}
-- | 'Just n' if the given expressions agree on 'n' inputs (they might also
-- | agree on fewer). 'Nothing' if it is determined that they won't agree; if
-- | that cannot be determined, the 'Delay' will never end.
extensionalInputs :: Com -> Com -> Delay (Maybe Natural)
extensionalInputs x y = race (agreeSym x y) >>= go 0
  where go !n (Same _  , m, _) = Now (Just (n+m))
        go !n (Diff a b, _, s) = if provablyDisagree (toCom a) (toCom b)
                                    then Now Nothing
                                    else race s >>= go (n+1)

agreeOnExtensionalInputs :: (Fuel, Com, Com, InputValues) -> Bool
agreeOnExtensionalInputs (n, x, y, inputs) =
    runDelayOr True (extensionalInputs x y >>= check) n
  where check Nothing  = Now True
        check (Just i) = same <$> sAt i (agree x y inputs)
```

```{.unwrap pipe="./run"}
main = checkPred agreeOnExtensionalInputs (tuple4 genFuel genCom genCom genComs)
```

This is easy to transform into a legitimate predicate for testing extensional
equality, that's able to answer both `True`{.haskell} and `False`{.haskell} (at
least, some of the time):

```{.haskell pipe="./show Main.hs"}
-- | Whether the given expressions are extensionally equal, i.e. cannot be
-- | distinguished by an SK expression.
extEq :: Com -> Com -> Delay Bool
extEq x y = isJust <$> extensionalInputs x y
```

`extEq`{.haskell} is a generalisation of our previous checks:

```{.haskell pipe="./show Main.hs"}
extEqGeneralisesEqAndNormalEqAndEverAgree :: (Fuel, Com, Com) -> Bool
extEqGeneralisesEqAndNormalEqAndEverAgree (n, x, y) =
    case ( go (            extEq x y)
         , go (        everAgree x y)
         , go (same <$> normalEq x y)
         ,                  (==) x y) of
      (Now False, Now True, _       , _   ) -> False
      (Now False, _       , Now True, _   ) -> False
      (Now False, _       , _       , True) -> False
      _                                     -> True
  where go = runDelay n
```

```{.unwrap pipe="./run"}
main = checkPred extEqGeneralisesEqAndNormalEqAndEverAgree
                 (tuple3 genFuel genCom genCom)
```

## Next time ##

We've seen how extensional equality works *in theory*, so the next post will
explore these definitions in more depth, to find out where I was going wrong in
egglog.
