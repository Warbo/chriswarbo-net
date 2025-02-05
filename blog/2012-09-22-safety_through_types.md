---
title: Safety through types
---
Hoare said that our software should obviously have no bugs, rather than merely having no obvious bugs.

Let's say our program needs to finds the square root of the combined lengths of two lists. This sounds pretty simple, and in a language like Javascript we could write the following:

```javascript
var sqrtOfLengths = function(x, y) {
    return Math.sqrt(x.length + y.length);
};
```

There's a lot of syntactic noise in there, but there's no obvious bugs. However, there is a huge potential for this code to contribute to a non-obvious bug, since it's making an awful lot of assumptions which could be completely wrong:

```javascript
// We assume that x and y are defined, but they might not be
sqrtOfLengths(); // x and y are undefined
sqrtOfLengths('abc'); // y is undefined
sqrtOfLengths({}[0], 'abc'); // x is undefined

// We also assume that x and y have a length
sqrtOfLengths(1, 2); // x and y don't have a length
sqrtOfLengths(1, 'abc'); // x doesn't have a length
sqrtfLengths('abc', 1); // y doesn't have a length

// We assume that the lengths can be added
sqrtOfLengths({length: function(){}}, {length: function(){}});

// We assume that the sum of the lengths can be passed to Math.sqrt
sqrtOfLengths({length: -10}, 'abc'); // Math.sqrt rejects -7
```

Now, the usual response to such examples will be that they're 'obviously wrong'. But in Javascript there's no such thing; interfaces are things which just-so-happen to be, they cannot be relied upon. Also, all non-syntactic errors only get picked up at runtime, if a branch happens to get called (this is why test suites' code coverage matters). This means no informative messages for the programmer, who may be able to fix it; just a big crash for the user, who has no idea what's just happened.

We can improve this by annotating our programs. Javascript doesn't let us do this, so I'll switch to Haskell. This Haskell code won't be the prettiest, but it will be the most straightforward:

```haskell
sqrtOfLengths x y = sqrt (fromIntegral (length x + length y))
```

This looks pretty much identical to our Javascript version, except we have `fromIntegral`{.haskell} in there and don't have Javascript's boilerplate `var`{.javascript}, `function`{.javascript} and `return`{.javascript} keywords. How is this any different? Well, the chaps who wrote the `length`{.haskell} function gave it a type:

```haskell
length :: [a] -> Int
```

This means "`length`{.haskell} is a thing which turns lists of some type into `Int`{.haskell}s". Now, this type isn't particularly great (since `Int`{.haskell}s can be negative, but `length`{.haskell}s can't) but it's better than our assumption-filled Javascript.

By taking the `length`{.haskell} of `x`{.haskell} and `y`{.haskell}, Haskell infers that they must be lists without us ever having to say it explicitly.

Addition has the type:

```haskell
(+) :: Num a => a -> a -> a
```

This means that it takes two values of the same type and spits out a value of that type, but only if the type implements the ``Num`{.haskell} interface. `Int`{.haskell} implements `Num`{.haskell}, so Haskell finds no problem with adding two `length`{.haskell}s.

The `sqrt`{.haskell} function is a bit crap, since it has the following type:

```haskell
sqrt :: Floating a => a -> a
```

This says that `sqrt`{.haskell} takes a value which implements the `Floating`{.haskell} interface and spits out another value of that type. This means that `sqrt`{.haskell} won't find the square root of an `Int`{.haskell} for us, since `Int`{.haskell} isn't `Floating`{.haskell}. We can perform an explicit conversion by passing our `Int`{.haskell} to `fromIntegral`{.haskell}, which turns anything of `Integral`{.haskell} type (like `Int`{.haskell}) into some other kind of `Num`{.haskell}:

```haskell
fromIntegral :: (Integral a, Num b) => a -> b
```

In this case, Haskell chooses to give out a `Floating`{.haskell} type, so that `sqrt`{.haskell} is happy.

After all of this checking, Haskell determines that our function has the type:

```haskell
sqrtOfLengths :: Floating c => [a] -> [b] -> c
```

This means it takes two lists (of potentially different things) and produces a `Floating`{.haskell} result.

This is much better than the Javascript version, since many assumptions we are making are enforced by the language:

 - We know that `x`{.haskell} and `y`{.haskell} are defined, since Haskell has been able to check that they are lists
 - We know that `length`{.haskell}s can be added, since they're `Int`{.haskell}s
 - We know that the sum can be passed to `sqrt`{.haskell}, since it will be positive

There are still some weaknesses here though:

 - If we're using an 'infinite' list, which Haskell allows us to define as long as we never ask for the whole thing, we can't find it's `length`{.haskell} (since this involves looking at the whole thing).
 - The type of `sqrt`{.haskell} is a bit off:
  - It's too specific. `Num`{.haskell}bers which aren't `Floating`{.haskell} can still have square roots.
  - It claims to turn one `Floating`{.haskell} value into another, which can't work. `Floating`{.haskell} is a sub-set of the positive and negative Rationals, as well as the nonsensical values `+infinity`, `-infinity` and `not a number`. The square root of a negative number is Imaginary, not Rational, not to mention that most square roots are Irrational.

The last point could be solved in a couple of ways; we could extend `sqrt`{.haskell} to give back Complex numbers, but in this case that's unnecessary. We know that the `length`{.haskell} of a list will be positive, even though its type doesn't say so. What we want is something like the following:

```haskell
data Natural = Zero | OnePlus Natural
class Positive
instance Positive Natural

length :: [a] -> Natural
sqrt :: (Positive a, Positive b) => a -> b -- for simplicity we only care about the positive root
```

The new types are quite naïve and inefficient here, but it will do for demonstration.

Now what about the infinite list problem? We could force Haskell to evaluate our lists before sending them to `length`{.haskell} (making `length`{.haskell}'s argument 'strict'). That would fix `length`{.haskell}, but the problem would just be shifted up the hierarchy to `sqrtOfLengths`{.haskell}, and if we make that strict it would just shift the problem to whoever calls us. We're heading in the right direction, since it puts the burden of finiteness on the programmer who's creating the list, but this may not be the person using our `sqrtOfLengths`{.haskell} function; we need something which will permeate the whole call stack, all the way back to wherever the problem originates. For this, of course, we need a new type.

By putting a finiteness condition on our argument, it means that anyone calling us can either take care of the finiteness condition themselves, or they can pass the buck and put finiteness in their own type. Here we use a 'type `class`{.haskell}' to identify finite lists; this is a contract which other types can fulfil (a bit like an interface, in languages like Java). A type `FiniteList l => l x`{.haskell} means a finite list (`l`{.haskell}) which contains elements of type `x`{.haskell}.

```haskell
-- For a type "l" to be a FiniteList it must have a "toList"
-- function, which turns an "l" full of "t"s into a list full
-- of "t"s
class FiniteList l where
  toList :: l t -> [t]

-- We can get the length of a FiniteList by turning it into a
-- regular list, then finding the length of that
finiteLength = length . toList -- "a . b" is a(b(...))

sqrtOfLengths xs ys = sqrt (finiteLength xs + finiteLength ys)
```

Our code will now refuse to compile if we don't prove that everything we give to `sqrtOfLengths`{.haskell} is finite.

How do we prove this? We use a common Haskell trick; we take a concept from the world of data and lift it to the world of types. The following types implement type-level singly-linked/tail-recursive lists:

```haskell
-- Empty lists are known as 'nil'
-- The left hand side has a type variable "t", which is the type of
-- elements we can add to this list (Integer, String, etc.)
-- The right hand side is the constant we will represent empty lists
-- with ("t" will be inferred automatically)
data Nil t = Nil

-- Prepending an element on to a list is known as a 'cons'
-- The left hand side has a type variable "t" for the elements, just
-- like Nil does, but it also has another, "l", which is the list
-- we're prepending to; remember, our lists are themselves types!
-- The right hand side tells us how to make a "Cons l t", we need to
-- use the constructor function "Cons", give it something of type "t"
-- (ie. an element to prepend) and something of type "l t", which is
-- the list we're prepending to. Note that by passing "t" through to
-- "l" like this, we force all elements in the list to have the same
-- (polymorphic) type.
data Cons l t = Cons t (l t)

-- Empty lists are finite
instance FiniteList Nil where
    -- Any Nil value is equivalent to an empty Haskell list
    toList Nil = []

-- An element prepended on to a finite list is finite
instance (FiniteList l) => FiniteList (Cons l) where
    -- A Cons value is equivalent to Haskell's ":"
    toList (Cons x xs) = x : toList xs
```

The trick here is that the `FiniteList`{.haskell} condition for `Cons`{.haskell} gets passed along to the list we're prepending to. The only thing that's finite unconditionally, and can therefore end the chain, is `Nil`{.haskell}. Knowing this at compile time is a sufficient and neccessary condition for using `length`{.haskell} safely. Of course, we could get the compiler stuck in an infinite loop while it's trying to prove the finiteness of an infinite list, but compile-time errors like this are much better than run-time errors.

While I was writing this I wanted to see if there were any generally-known solutions to the finite list problem, and came across the question [How to tell if a list is infinite?](http://stackoverflow.com/questions/7371730/how-to-tell-if-a-list-is-infinite) on [StackOverflow](http://www.stackoverflow.com). All of the answers showed this to be an instance of the [Halting Problem](http://en.wikipedia.org/wiki/Halting_Problem), which is technically correct but misses the point. We don't need to know if a list is infinite; we need to know if it's finite, and ignore anything which fails (which includes all infinite lists and some finite ones). I think a rant about Constructivism is in order! My response is [here](http://stackoverflow.com/questions/7371730/how-to-tell-if-a-list-is-infinite) :)

### UPDATE: ###

I fixed some typos and added the quick-n-dirty `Natural`{.haskell} implementation. While adding that, it of course occured to me that we can define provably-finite naturals:

```haskell
class FiniteNat n where
  toInteger :: n -> Integer

data Zero = Zero
data OnePlus n = OnePlus n

instance FiniteNat Zero where
  toInteger Zero = 0

instance (FiniteNat n) => FiniteNat (OnePlus n) where
  toInteger (OnePlus n) = 1 + toInteger n
```

This is exactly the same as the lists, except we don't have to care about containing elements. Of course, that naïve implementation of `toInteger`{.haskell} for `OnePlus`{.haskell} can easily cause a stack overflow, so we should do something more like:

```haskell
  toInteger (OnePlus n) = foldr (+) 1 [0..n]
```

I'm currently throwing a small library together for provably-finite and provably-infinite types.

PS: The similarities between Naturals and Lists are extremely apparent when we play around with [Ornaments](http://personal.cis.strath.ac.uk/~conor/pub/OAAO/Ornament.pdf) :)
