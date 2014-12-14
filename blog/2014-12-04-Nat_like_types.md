---
title: Nat-like Types, or Dynamic and Static Information
---
This is based on [an explanation I gave][mymail] on the [Idris mailing list][idrismail] about using dependent types to prove that a Vector contains some element.

This post assumes some familiarity with the `Nat`{.haskell} type. If you've not encountered it before, take a look at [Peano arithmetic][peano]

## `Nat`{.haskell} ##

`Nat`{.haskell} has two constructors:

```haskell
Z : Nat
S : Nat -> Nat
```

These can be combined in an infinite number of ways: `Z`{.haskell}, `S Z`{.haskell},
`S (S Z)`{.haskell}, etc.

From a static (typing) point of view, `Nat`{.haskell} doesn't tell us very much at
all, since it can always be trivially satisfied with `Z`{.haskell} (`Z`{.haskell} is a lot
like `()`{.haskell} or `NULL`{.java}).

From a dynamic (value) point of view, the only thing a `Nat`{.haskell} tells us is
how many `S`{.haskell} constructors it has.

Hence, we can think of `Nat`{.haskell} as being the Natural numbers (the 'counting
numbers', where we're counting how many `S`{.haskell} wrappers there are around
`Z`{.haskell}).

We can define types similar to `Nat`{.haskell} which are more informative, both
statically (stronger types) and dynamically (contain more data).

## `Exactly n`{.haskell} ##

We can define a strange type `Exactly n`{.haskell} which is like `Nat`{.haskell}, except it
stores a `Nat`{.haskell} in its type too:

```haskell
data Exactly : Nat -> Type where
  eZ : Exactly Z
  eS : Exactly n -> Exactly (S n)
```

Dynamically, an `Exactly n`{.haskell} value is like a `Nat`{.haskell}: all we can
use it for is counting. We can see this if we throw away the static
information:

```haskell
plain : Exactly n -> Nat
plain eZ = Z
plain (eS n) = S (plain n)
```

However, we have more static information: an `Exactly n`{.haskell} value will have
`n`{.haskell} occurences of the `eS`{.haskell} constructor, hence it guarantees the
following:

```haskell
guarantee : n -> Exactly n -> Bool
guarantee n e = plain e == n
```

In other words, `Exactly n`{.haskell} is like 'all Naturals which are equal to `n`{.haskell}'.

## `Fin t`{.haskell} ##

We can change our constructors slightly to make a useful type called
`Fin n`{.haskell}, which is a lot like `Exactly n`{.haskell} except for the guarantee that
it provides. It has two constructors, just like `Nat`{.haskell} and `Exactly n`{.haskell}:

```haskell
fZ : Fin (S n)
fS : Fin n -> Fin (S n)
```

Once again, we can throw away the type information to get back to `Nat`{.haskell}:

```haskell
plain : Fin n -> Nat
plain  fZ    = Z
plain (fS n) = S (plain n)
```

Hence, dynamically a `Fin n`{.haskell} is also just a counter like `Nat`{.haskell}.

Statically, the guarantee we get from `Fin n`{.haskell} is that it has exactly `n`{.haskell}
possible values (try it for `n`{.haskell} = `0`{.haskell}, `1`{.haskell}, `2`{.haskell}, ... and see!).

The extra type information guarantees that:

```haskell
fin : n -> Fin n -> Bool
fin n f = plain f < n
```

Hence we can think of `Fin n`{.haskell} as being like 'the Naturals up to `n`{.haskell}'. For
more info, see [this informative StackOverflow answer](http://stackoverflow.com/questions/18726164/how-can-finite-numbers-work-dependent-types)

## `NatLTE n m`{.haskell} ##

`NatLTE n m`{.haskell} is like `Exactly n`{.haskell} but it stores *two* `Nat`{.haskell} arguments in
its type. Again, its constructors follow the same basic pattern as `Nat`{.haskell}:

```haskell
nEqn : NatLTE n n
nLTESm : NatLTE n m -> NatLTE n (S m)
```

We can throw away the type info to get:

```haskell
plain : NatLTE n m -> Nat
plain nEqn = Z
plain (nLTESm n) = S (plain n)
```

Notice that again, dynamically we're just left with a counter. However,
we have some static guarantees about the value of that counter.

Notice that the two `Nat`{.haskell} values in our type must start out the same,
although unlike `eZ`{.haskell} they can start at any value. Just like `eS`{.haskell} and
`fS`{.haskell}, we add an `S`{.haskell} with the `nLTESm`{.haskell} wrapper, although we also have
another `Nat`{.haskell} which gets passed along unchanged.

The guarantee for `NatLTE n m`{.haskell} is:

```haskell
guarantee : n -> m -> NatLTE n m -> Bool
guarantee n m x = n + (plain x) == m
```

Hence we can think of `NatLTE n m`{.haskell} as being '`Nat`{.haskell}s which, when added to
`n`{.haskell}, produce `m`{.haskell}' or equivalently 'proof that `n`{.haskell} is less than or equal to `m`{.haskell}'.

## `Elem x xs`{.haskell} ##

`Elem x xs`{.haskell} is like `NatLTE n m`{.haskell} except its type can contain more than
just `Nat`{.haskell} values. Again, its constructors follow the same basic pattern
as `Nat`{.haskell}:

```haskell
Here : Elem x (x::xs)
There : Elem x xs -> Elem x (y::xs)
```

We can throw away the type information to get a `Nat`{.haskell}:

```haskell
plain : Elem x xs -> Nat
plain Here = Z
plain (There n) = S (plain n)
```

Again, dynamically a value of `Elem x xs`{.haskell} is only useful as a
counter. The static guarantee we get is the following:

```haskell
guarantee : x -> xs -> Elem x xs -> Bool
guarantee x xs e = index' (plain e) xs == Just x
```

It does this by using `Here : Elem x xs`{.haskell} to tells us that `x`{.haskell} is the
head of `xs`{.haskell}, whilst `There`{.haskell} tells us that `x`{.haskell} appears somewhere in the
tail of `xs`{.haskell}.

Hence we can think of `Elem x xs`{.haskell} as being 'proof that `x`{.haskell} is somewhere in
`xs`{.haskell}'.

We get compile errors if we change the `Here`{.haskell} and `There`{.haskell} values in a program, because we're changing the proof. Since the proof tells Idris what
the index of the element is, changing that index makes the proof wrong and the compiler rejects it.

## `List t`{.haskell} ##

Note that `List t`{.haskell} also follows the same constructor pattern as `Nat`{.haskell}:

```haskell
Nil : List t
Cons x : List t -> List t
```

The argument to `Cons`{.haskell} (the `x`{.haskell}) adds *dynamic* data to the basic
"count the constructors" information found in `Nat`{.haskell}. Note that we gain
no static information, regardless of what `t`{.haskell} is, since we might always
be given a `Nil`{.haskell}.

We can throw away the dynamic information to get a `Nat`{.haskell}:

```haskell
plain : List t -> Nat
plain Nil = Z
plain (Cons _ xs) = S (plain xs)
```

In the same way that we went from `Nat`{.haskell} to `Fin n`{.haskell}, you might try making
a new type `FinList n t`{.haskell} for 'all lists of `t`{.haskell} up to length `n`{.haskell}'.

In the same way that we went from `Fin n`{.haskell} to `NatLTE n m`{.haskell}, you might try
making a new type `SuffixList l1 l2`{.haskell} for 'proofs that `l1`{.haskell} is a suffix of
`l2`{.haskell}'.

## `Vect n t`{.haskell} ##

`Vect n t`{.haskell} itself is a bit like `FinList n t`{.haskell}, except instead of
containing all lists up to length `n`{.haskell}, it contains *only* lists of
length `n`{.haskell}. It follows the same constructor pattern as `Nat`{.haskell} and
`List t`{.haskell}:

```haskell
Nil : Vect Z t
Cons x : Vect n t -> Vect (S n) t
```

Notice that the `Nat`{.haskell} in the type of `Nil`{.haskell} is forced to be `Z`{.haskell}, instead
of being `S n`{.haskell} for all `n`{.haskell} like in `Fin`{.haskell} (hint, that's the difference
between `Vect`{.haskell} and `FinList`{.haskell} ;) )

If we throw away the static information of `Vect n t`{.haskell} we get a `List t`{.haskell}:

```haskell
plain : Vect n t -> List t
plain Nil = Nil
plain (Cons x xs) = Cons x (plain xs)
```

If we throw away the dynamic information, we get an `Exactly n`{.haskell}:

```haskell
plain : Vector n t -> Exactly n
plain Nil = eZ
plain (Cons _ xs) = eS (plain xs)
```

## Conclusion ##

We can add dynamic information (values) to a type by adding arguments to
the constructors; this takes us, for example, from `Nat`{.haskell} to `List t`{.haskell}.

We can also add static information to a type by adding arguments to the
*type* ("strengthening" it), and use the constructors to guarantee some
invariant between all these values (ie. we don't provide a way to construct an invalid value).

The simple pattern of two constructors: one which gives a `Foo`{.haskell} and
another which turns a `Foo`{.haskell} into a `Foo`{.haskell}, is very powerful, and can
represent all kinds of 'linear' things, including counting, lists,
bounds, linear relationships, lengths, indexes, etc.

[mymail]: https://groups.google.com/d/msg/idris-lang/r4Y7FOTOF5w/gG81lUoBijkJ
[idrismail]: https://groups.google.com/forum/#!forum/idris-lang
[peano]: http://en.wikipedia.org/wiki/Peano_axioms
