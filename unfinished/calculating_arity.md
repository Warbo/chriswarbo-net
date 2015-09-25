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

```{.haskell pipe="sh"}
runhaskell < multiparamval.hs 2>&1
true
```

Type-level programming in Haskell can *only* involve types, not regular values like numbers, so we need some type-level notion of numbers instead. In other words, an arity `n`{.haskell} is both a *type* and a *number*; it's not a "numeric type", like `Int`{.haskell}, since `Int`{.haskell} itself is not a number; likewise it's not an element of a numeric type like `Int`{.haskell}, like `3 :: Int`{.haskell}, since `Int`{.haskell}s are not types. We need a new notion, which satisfies both of these requirements:

```{.haskell pipe="tee -a multiparamtype.hs fundep.hs"}
-- Type level Peano numerals
data Zero   = Z
data Succ n = S n

```

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

```{.haskell pipe="sh"}
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

```
{pipe="sh"}
runhaskell -XMultiParamTypeClasses  \
           -XFunctionalDependencies \
           -XFlexibleInstances      \
           -XUndecidableInstances < fundep.hs
```

# Type Families

Type families are currently popular, as an alternative to multiparameter typeclasses. I originally gave up trying to understand these, after banging my head against GHC for a while. I was recently inspired to have another go, after attending a talk by Lennart AugustssonFIXME which involved some pretty hairy type-level hackery. During the course of his presentation, type families finally clicked for me, and I was able to bang out the following approach:

```{.haskell pipe="tee -a typefamily.hs"}
type family Arity :: * -> * where
  Arity (i -> o) = Succ (Arity o)
  Arity _        = Zero

```

This defines `Arity`{.haskell} as a *closed type family*. A type family is basically a type-level *function* (unlike the type-level *relations* encoded by multiparameter typeclasses). Type families can be *open*, allowing new pattern clauses to be added at any point, provided they don't overlap (just like typeclass instantiation); alternatively they can be *closed*, where a fixed set of, potentially overlapping, pattern clauses are used (just like function definition). We use the latter, to allow our "catch-all" pattern to overlap with the `i -> o`{.haskell} pattern.

With this type family, we can finally get the arity of each type. However, one problem still remains: the result will be a *type-level* number, when we'd really like a regular `Int`{.haskell}. Since each type-level number is a different type, we can use a regular type class to obtain their associated `Int`{.haskell}s:

```{.haskell pipe="tee -a typefamily.hs"}
class IsNum a where
  num :: a -> Int

instance IsNum Zero where
  num Z = 0

instance (IsNum n) => IsNum (Succ n) where
  num (S n) = 1 + (num n)

```

```{pipe="sh"}
# Try compiling every Haskell file
for FILE in *.hs
do
  echo -e "\nmain = return ()" > "$FILE"
  ghc --make "$FILE"
done
```
