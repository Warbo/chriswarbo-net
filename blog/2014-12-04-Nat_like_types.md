---
title: Nat-like Types, or Dynamic and Static Information
---
This is based on [an explanation I gave][mymail] on the [Idris mailing list][idrismail] about using dependent types to prove that a Vector contains some element.

# The Skeleton #

All of the types in this post will be variations of the `Nat`{.haskell} type. If you've not encountered it before, take a look at [Peano arithmetic][peano].

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

# Static Information #

These types start with `Nat`{.haskell} and add *static* information (ie. stronger types).

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

# Dynamic Information #

These types start with `Nat`{.haskell} and add *dynamic* information (ie. data).

## `List t`{.haskell} ##

`List t`{.haskell} follows the same constructor pattern as `Nat`{.haskell}:

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

## `Delay t`{.haskell} ##

`Delay t`{.haskell} is like `List t`{.haskell}, except it can only store data in the
`Nil`{.haskell} constructor, not the `Cons`{.haskell} constructor. For this reason, we rename
the constructors to `Now`{.haskell} and `Later`{.haskell}, respectively:

```haskell
Now x : Delay t
Later : Delay t -> Delay t
```

If we throw away the dynamic information, we get a counter like `Nat`{.haskell}:

```haskell
plain : Delay t -> Nat
plain (Now   x) = Z
plain (Later x) = S (plain x)
```

What about the static information? `List t`{.haskell} was trivial for all `t`{.haskell}, since
we can satisfy it with `Nil`{.haskell}. We can't do that with `Delay t`{.haskell}, since
`Now`{.haskell} requires an argument of type `t`{.haskell}.

In fact, we *can* satisfy `Delay t`{.haskell} trivially, by using `Later`{.haskell}:

```haskell
loop : Delay t
loop = Later loop
```

This defines a never-ending chain of `Later (Later (Later (Later ...)))`{.haskell}. Since we never
terminate the chain with a `Now`{.haskell}, we don't have to provide a value of type `t`{.haskell}.

This kind of circular reasoning is called *co-induction*, and is logically sound as long as each
constructor can be determined by a terminating function (in this case, we just step along the chain
until we reach the desired depth, pattern-matching the (constant) constructors as we go). This
condition is called *co-termination*.

If a function doesn't terminate or co-terminate, it *diverges*. We can use `Delay t`{.haskell} to
handle diverging functions in a safe way. For example, the Halting Problem tells us that it's
impossible to calculate whether a Universal Turing Machine will halt for arbitrary inputs; ie. the
UTM might *diverge*. However, we can always perform each *step* of the UTM in a finite amount of
time.

If we wrap the result of our UTM in a `Delay`{.haskell}, we can return a `Later`{.haskell} for each
step, and if it does eventually halt we can return a `Now`{.haskell}. Whenever it diverges, our
program doesn't freeze; instead, we get the same `Later (Later (Later ...))`{.haskell} chain as
`loop`{.haskell}, which we can inspect to various depths.

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
