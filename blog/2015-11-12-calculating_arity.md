---
title: Calculating Arity
---

The *arity* of a value is the number of arguments it can accept. We can define arity by pattern-matching on types (a feature known as *typecase*):

```haskell
arity (I -> O) = 1 + arity O
arity  T       = 0
```

However, it's difficult to *implement* this kind of type-level function. Here are some approaches I tried in Haskell.

# Type Classes #

We want `arity`{.haskell} to have a different value, depending on the type we're using. We can do this by *overloading* `arity`{.haskell} using type classes:

```{.haskell pipe="tee -a class.hs"}
class HasArity a where
  arity :: Int

```

However, we hit problems if we try to *implement* this type class:

```{.haskell pipe="tee -a class.hs"}
instance (HasArity o) => HasArity (i -> o) where
  arity = 1 + arity

instance HasArity t where
  arity = 0

```

There are two issues here. Firstly, we don't know which `arity`{.haskell} to use in the expression `1 + arity`{.haskell}:

```{pipe="sh"}
runhaskell < class.hs 2>&1
true
```

We can fix this by turning `arity`{.haskell} into a function:

```{.haskell pipe="tee -a class2.hs"}
class HasArity a where
  arity :: a -> Int

instance (HasArity o) => HasArity (i -> o) where
  -- undefined inhabits every type, including i
  arity f = 1 + arity (f undefined)

instance HasArity t where
  arity _ = 0

```

The other problem is that these two instances *overlap*; the first is just a special case of the second:

```{.haskell pipe="tee -a class2.hs"}
main = print (arity not)

```

```{pipe="sh"}
runhaskell -XFlexibleInstances < class2.hs 2>&1
true
```

Unlike pattern-matching, which tries alternatives from top to bottom, type classes don't have such an ordering. This makes it difficult to implement a "catch-all" instance which works for any type *other* than functions.

# Multi-Parameter Type Classes #

The classic way to implement type-level computation in Haskell is to use multi-parameter type classes as a form of logic programming. Similar to Prolog, to represent a total function of the form `arity x = a`{.haskell}, we instead define a *partial* relation of the form `arity x a`{.haskell}. We can then implement that relation as a multi-parameter type class, relating the input type to its arity:

```{.haskell pipe="tee -a multiparamval.hs multiparamtype.hs"}
class HasArity a n where
  arity :: a -> n

```

Unfortunately, Haskell doesn't have dependent types, so we can't represent arities with regular numbers:

```{.haskell pipe="tee -a multiparamval.hs"}
instance (HasArity o n) => HasArity (i -> o) (1 + n) where
  arity f = 1 + arity (f undefined)

instance HasArity t 0 where
  arity _ = 0

```

```{pipe="sh"}
runhaskell < multiparamval.hs 2>&1
true
```

Type-level programming in Haskell can *only* involve types, not regular values like numbers, so we need some type-level notion of numbers instead. In other words, an arity `n`{.haskell} is both a *type* and a *number*; it's not a "numeric type", like `Int`{.haskell}, since `Int`{.haskell} itself is not a number; likewise it's not an element of a numeric type like `Int`{.haskell}, like `3 :: Int`{.haskell}, since `Int`{.haskell}s are not types. We need a new notion, which satisfies both of these requirements:

```{.haskell pipe="tee -a multiparamtype.hs fundep.hs typefamily.hs"}
-- Type level Peano numerals
data Zero   = Z
data Succ n = S n

```

To make life easier, we provide a `toInt`{.haskell} function to turn these Peano numbers into regular `Int`{.haskell}s:

```{.haskell pipe="tee -a multiparamtype.hs fundep.hs typefamily.hs"}
class ToInt a where
  toInt :: a -> Int

instance ToInt Zero where
  toInt _ = 0

instance (ToInt n) => ToInt (Succ n) where
  toInt (S x) = 1 + toInt x

```

Now we can return these Peano numbers from our `arity`{.haskell} functions:

```{.haskell pipe="tee -a multiparamtype.hs"}
instance (HasArity o n) => HasArity (i -> o) (Succ n) where
  arity f = S (arity (f undefined))

instance HasArity t Zero where
  arity _ = Z

```

This gets us part of the way there; our arities are now living at the same level as our types, but now we have a couple of problems regarding ambiguity:

```{.haskell pipe="tee -a multiparamtype.hs"}
main = print (arity not)

```

```{pipe="sh"}
runhaskell -XMultiParamTypeClasses  \
           -XFunctionalDependencies \
           -XFlexibleInstances      \
           -XFlexibleContexts < multiparamtype.hs 2>&1
true
```

# Functional Dependencies #

These ambiguities are due to the *open* nature of type classes: we can declare new instances at any point, so the fact that we just-so-happen to have instances with `Zero`{.haskell} and `Succ n`{.haskell} as the second parameter doesn't actually tell Haskell very much; it might come across other definitions elsewhere which use other types as the second parameter, and hence there is ambiguity.

To make the second parameter unambiguous we can use *functional dependencies* to indicate that the second parameter is completely determined by the first one:

```{.haskell pipe="tee -a fundep.hs"}
class HasArity a n | a -> n where
  arity :: a -> n

```

Now we can make our two instances:

```{.haskell pipe="tee -a fundep.hs"}
instance (HasArity o n) => HasArity (i -> o) (Succ n) where
  arity f = S (arity (f undefined))

instance HasArity t Zero where
  arity _ = Z

```

However, we still have an ambiguity problem; the "catch-all" parameter `t`{.haskell} of the second instance overlaps with the more-specific function type `i -> o`{.haskell} of the first:

```{pipe="sh"}
runhaskell -XMultiParamTypeClasses  \
           -XFunctionalDependencies \
           -XFlexibleInstances      \
           -XUndecidableInstances < fundep.hs 2>&1
true
```

# Type Families

Type families are the solution we've been looking for. I originally gave up trying to understand these, after banging my head against GHC for a while. I was recently inspired to have another go, after attending a talk by Lennart Augustsson which involved some pretty hairy type-level hackery. During the course of his presentation, type families finally clicked for me, and I was able to bang out the following approach:

```{.haskell pipe="tee -a typefamily.hs"}
type family Arity t :: * where
  Arity (i -> o) = Succ (Arity o)
  Arity t        = Zero

```

This defines `Arity`{.haskell} as a *closed type family*. A type family is basically a type-level *function* (unlike the type-level *relations* encoded by multiparameter typeclasses). Type families can be *open*, allowing new pattern clauses to be added at any point, provided they don't overlap (just like typeclass instantiation); alternatively they can be *closed*, where a fixed set of, potentially overlapping, pattern clauses are used (just like function definition). We use the latter, to allow our "catch-all" pattern to overlap with the `i -> o`{.haskell} pattern.

With this type family, we can finally get the arity of each type. However, one problem still remains: the result will only be accessible at the *type-level*. To associate a value with each `Arity`{.haskell} type, we can use type classes; we no longer have problems with ambiguity, since we can treat all arities in the same way.

First we use the fact that our type-level numbers are *singletons*, i.e. they only contain one value (ignoring `undefined`{.haskell}). We use a type class to associate the name `singleton :: a`{.haskell} with the single value of type `a`{.haskell}:

```{.haskell pipe="tee -a typefamily.hs"}
class Singleton a where
  singleton :: a

instance Singleton Zero where
  singleton = Z

instance (Singleton n) => Singleton (Succ n) where
  singleton = S singleton

```

We can use the `Singleton`{.haskell} class to write our `arity`{.haskell} function. All of our calculation is performed by `Arity`{.haskell} at the type level, which results in either `Zero`{.haskell} or `Succ n`{.haskell} for some `n`{.haskell}; we use `singleton`{.haskell} to access the (only) value of that result type:

```{.haskell pipe="tee -a typefamily.hs"}
arity :: (Singleton (Arity a)) => a -> Arity a
arity _ = singleton

-- Handy helper functions

-- Return the arity of a value, as an Int
intArity :: (Singleton (Arity a), ToInt (Arity a)) => a -> Int
intArity = toInt . arity

-- Pretty output
format s x = concat ["Arity of '", s, "' is ", show (intArity x)]

```

Now, at last, we can get the arity of values:

```{pipe="sh > /dev/null"}
cp typefamily.hs typefamily2.hs
cp typefamily.hs typefamily3.hs
```

```{.haskell pipe="tee -a typefamily.hs"}
main = mapM_ putStrLn [
    format "Bool -> Bool"           not
  , format "String"                 "hello"
  , format "Bool -> Bool -> Bool"   (&&)
  , format "(a -> b) -> [a] -> [b]" map
  ]

```

```{pipe="sh"}
runhaskell -XTypeFamilies -XFlexibleContexts < typefamily.hs
```

It's also interesting to note the values which *don't* work. For example, we fail to get the arity of the identity function `id :: a -> a`{.haskell}:

```{.haskell pipe="tee -a typefamily2.hs"}
main = putStrLn (format "a -> a" id)

```

```{pipe="sh"}
runhaskell -XTypeFamilies -XFlexibleContexts < typefamily2.hs 2>&1
true
```

This is because the output type of `id`{.haskell} is completely polymorphic: it could be anything, *including a function*. We know the arity must be *at least* 1, but it could be arbitrarily higher; we don't have enough information to know, and hence the compiler can't finish the calculation.

To get around this, we can restrict the output type of `id`{.haskell}. We don't have to completely monomorphise it, we just need to provide enough information to perform the type-level pattern-match for `Arity`{.haskell}:

```{.haskell pipe="tee -a typefamily3.hs"}
main = mapM_ putStrLn [
    format "Bool       -> Bool      " (id :: Bool       -> Bool)
  , format "[a]        -> [a]       " (id :: [a]        -> [a])
  , format "Maybe a    -> Maybe a   " (id :: Maybe a    -> Maybe a)
  , format "(a -> [b]) -> (a -> [b])" (id :: (a -> [b]) -> (a -> [b]))
  ]

```

```{pipe="sh"}
runhaskell -XTypeFamilies -XFlexibleContexts < typefamily3.hs
```

```{pipe="sh 1>&2"}
# Try compiling/type-checking every Haskell file
for FILE in *.hs
do
  echo -e "\nmain = return ()" > "$FILE"
  nix-shell -p haskellPackages.ghc --run "ghc --make '$FILE'"
done
```
