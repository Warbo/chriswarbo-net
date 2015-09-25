---
title: Calculating Arity
---

The *arity* of a value is the number of arguments it can accept. We can define arity by pattern-matching on types:

```haskell
arity (I -> O) = 1 + arity O
arity  T       = 0
```

However, it's difficult to *implement* this kind of type-level function. Here are some approaches I tried in Haskell.

# Type Classes #

We want `arity` to have a different value, depending on the type we're using. We can do this by *overloading* `arity` using type classes:

```haskell
class HasArity a where
  arity :: Int
```

However, we hit problems if we try to *implement* this type class:

```haskell
instance (HasArity o) => HasArity (i -> o) where
  arity = 1 + arity

instance HasArity t where
  arity = 0
```

There are two issues here. Firstly, we don't know which `arity` to use in the expression `1 + arity`. We can fix this by turning `arity`{.haskell} into a function:

```haskell
class HasArity a where
  arity :: a -> Int

instance (HasArity o) => HasArity (i -> o) where
  -- undefined inhabits every type, including i
  arity f = 1 + arity (f undefined)

instance HasArity t where
  arity _ = 0
```

The other problem is that these two instances *overlap*: the first is just a special case of the second. Unlike pattern-matching, which tries alternatives from top to bottom, type classes don't have such an ordering. This makes it difficult to implement a "catch-all" instance which works for any type *other* than functions.

# Multi-Parameter Type Classes #

The classic way to implement type-level computation in Haskell is to use multi-parameter type classes as a form of logic programming. Similar to Prolog, to represent a total function of the form `arity x = a`{.haskell}, we instead define a *partial* relation of the form `arity x a`{.haskell}. We can then implement that relation as a multi-parameter type class, using "functional dependencies" (the `a -> n` part) to indicate that there is at most one `n` for any particular `a`:

```{.haskell pipe="tee multiparam.hs"}
class HasArity a n | a -> n
  arity :: a -> n
```

Of course, by moving the definition into the type-level, we now need some notion of type-level numbers. In other words, `n` is both a *type* and a *number*; it's not a "numeric type", like `Int`, since `Int` itself is not a number; likewise it's not an element of a numeric type like `Int`, like `3 :: Int`, since `Int`s are not types. We need a new notion, which satisfies both of these requirements:

```
-- Type level Peano arithmetic
data Zero   = Z
data Succ a = S a

instance (HasArity o n) => HasArity (i -> o) (Succ n)
instance HasArity t Zero
```

This gets us part of the way there; our arities are now living at the same level as our types, but we still have the problem of overlapping instances.

I tried to use type families instead, since it's currently fashionable to replace multi-parameter type classes with them, but I gave up after banging my head against GHC for a while.

I was inspired to have another go, after attending a talk by Lennart AugustssonFIXME which involved some pretty hairy type-level hackery. During the course of his presentation, type families finally clicked for me, and I was able to bang out the following approach:

```{pipe="cat > multiparam.hs"}
main = return ()
```

```{pipe="sh"}
ghc --make multiparam.hs
```
