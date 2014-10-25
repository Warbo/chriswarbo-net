---
title: Typed Combinators
---
How can we specify types without using variables? We can create
Universal languages without variables by using combinators. Can
we create a universal set of combinators for types? Probably.
What would they look like? Well, let's focus on dependent types,
since they're the most interesting. There's an extension of
untyped Combinatory Logic called Illative Combinatory Logic
which adds types and annotations. In the paper 'Systems of
Illative Combinatory Logic Complete For First-Order
Propositional And Predicate Calculus' by Barendregt, Bunder and
Dekkers, there are a few different systems considered; they all
have the S and K combinators familiar from Schoenfinkel:

```haskell
data UnsafeCom = cK
               | cS
```

This syntax looks like Haskell, but I'll actually be using
[Idris] [1]. It looks similar to Haskell, but has dependent types.
Since Idris uses 'S' as the 'successor' function for Peano-style
Natural numbers, I've prefixed my constructors with 'c' for
Combinator. The reason they're 'Unsafe' is because, at the moment,
we're not doing any type-checking.

[1]: http://idris-lang.org

We need to add two new combinators for the type system, L and
&Xi; (which I'll shamefully write as X):

```haskell
                | cL
                | cX
```

I'll explain these individually below. While the raw combinators
are all in place, life is easier if we add some extra,
redundant combinators to help with our notation. These are:

```haskell
                | cF
                | cG
                | cH
                | cP
```

We can, as usual, apply one combinator to another:

```haskell
                | (@) UnsafeCom UnsafeCom
infixl 10 @
```

That last line just lets us write `x @ y @ z`{.haskell} instead of
`((@) ((@) x y) z`{.haskell}.

The derived combinators can be constructed as follows:

```haskell
P x y = X(Kx)(Ky)

F x y z = Xx(S(Ky)z)

G x y z = Xx(Syz)

H = S(KL)K
```

The &Xi; combinator is clearly popular. It is a form of type
annotation: '&Xi;xy' says that 'given an x, we can get a y'. This is
actually quite powerful, since it can denote subtyping (x is a
subtype of y), dependent functions like Gxyz, which sends any input
to 'z' through 'y' as well, sending the resulting type to G for
annotation. Non-dependent functions are typed with F (or Gx(Ky)z).

The type of types is L, so 'Lx' tells us that 'x' is a type. By
carefully restricting what we are allowed to derive, we can
avoid dangerous type-of-type-of-type scenarios (tackled with
'universes' in other systems). For example, if a function takes
types as arguments and return values, we don't annotate it
directly; we only annotate its argument and result, since they
fit in the level described by L.

Okay, time to see how these work. There are no new
beta-reduction rules, so we just have the familiar S and K
rules:

```haskell
reduce     (cK @ x @ y) = x
reduce (cS @ x @ y @ z) = x @ z @ (y @ z)
reduce          (x @ y) = (reduce x) @ (reduce y)
reduce                x = x
```

Now we can add our typing rules. These take the form of natural
deduction rules, which we encode as dependent function types.
The idea is that we have a 'context' of known terms, which we
can use to construct new terms. The construction rules are
functions from contexts to terms; however, this is similar
enough to Idris's type-checker that we can keep the context
implicit.

Here we make a `Typed`{.haskell} datatype which wraps 'unsafe'
combinators. By making the constructors dependent functions, we
can enforce our rules:

```haskell
data WellTyped : UnsafeCom -> Set where
```

The first typing rule is that anything in the context can be
derived. Since the Combinatory Logic context is the same as the
Idris context, this is just the identity function:

```haskell
  Id : WellTyped c -> WellTyped c
```

The next rule is that beta-reduction of a well-typed term gives
us a well-typed term. Here `x = y`{.haskell} is a type. This is
common in dependently typed programs; it acts as proof that the
left and right are identical, since it's only constructor is
`refl x : x = x`{.haskell}. Without a value of this type, we
can't run the function:

```haskell
  Beta : WellTyped c -> (reduce c = c') -> WellTyped c'
```

Now the rules become specific to the system we're using. The
following rules apply to the 'IP' system, which uses the P and H
combinators for convenience.

`cP @ x @ y`{.haskell} encodes that `x`{.haskell} implies
`y`{.haskell}. Therefore, given `cP @ x @ y`{.haskell} and
`x`{.haskell} we can imply `y`{.haskell}; decoding the information
in the type:

```haskell
  Pe : WellTyped (cP @ x @ y) -> WellTyped x -> WellTyped y
```

We can also go the other way. If the existence of `x`{.haskell}
would imply `y`{.haskell}, and `x`{.haskell} is a proposition
(encoded as `cH @ x`{.haskell}) then we can derive
`cP @ x @ y`{.haskell}. Because `x`{.haskell} may not exist, we
encode `if x then y`{.haskell} as a function taking `x`{.haskell}
as an argument. This is the standard way to construct implications
in type theory:

```haskell
  Pi : (WellTyped x -> WellTyped y)
       -> WellTyped (cH @ x)
       -> WellTyped (cP @ x @ y)
```

Finally if, given a proposition `x`{.haskell}, `y`{.haskell} is a
proposition, then `cP @ x @ y`{.haskell} is a proposition:

```haskell
  PH : (WellTyped x -> WellTyped (cH @ y))
       -> WellTyped (cH @ x)
       -> WellTyped (cH @ (cP @ x @ y))
```

This lets us use propositional logic with combinators; however,
it's a far cry from the rich type system that we want. The next-
most-sophisticated system is called 'I&Xi;' and makes direct use
of the &Xi; and L combinators.

If having an `x`{.haskell} implies having a `y`{.haskell}, and
`v`{.haskell} is an `x`{.haskell}, then `v`{.haskell} is also a
`y`{.haskell}:

```haskell
  Xe : WellTyped (cX @ x @ y)
          -> WellTyped (x @ v)
          -> WellTyped (y @ v)
```

If, given `a`{.haskell} of type `x`{.haskell}, `a`{.haskell} has
type `y`{.haskell}, then `x`{.haskell} is a sub-type of
`y`{.haskell}:

```haskell
  Xi : (WellTyped (x @ a) -> WellTyped (y @ a))
          -> WellTyped (cL @ x)
          -> WellTyped (cX @ x @ y)
```

If, given `a`{.haskell} of type `x`{.haskell}, we know
`ya`{.haskell} is a proposition, then `x`{.haskell} being a
sub-type of `y`{.haskell} is also a proposition:

```haskell
   XH : (WellTyped (x @ a) -> WellTyped (cH @ (y @ a)))
          -> WellTyped (cL @ x)
          -> WellTyped (cH @ (cX @ x @ y))
```

This is more powerful since we can talk about the relationships
between types, but we still don't get a recognisable type-system
since, for example, we don't really have functions. That's what
system 'IF' brings to the table.

If `z`{.haskell} is a function from `x`{.haskell} to
`y`{.haskell}, and `v`{.haskell} has type `x`{.haskell}, then
`zv`{.haskell} has type `y`{.haskell}:

```haskell
  Fe : WellTyped (cF @ x @ y @ z)
       -> WellTyped (x @ v)
       -> WellTyped (y @ (z @ v))
```

If, given `a`{.haskell} of type `x`{.haskell}, `za`{.haskell} has
type `y`{.haskell}, then `z`{.haskell} has type `x -> y`{.haskell}:

```haskell
  Fi : (WellTyped (x @ a) -> WellTyped (y @ (z @ a)))
       -> WellTyped (cL @ x)
       -> WellTyped (cF @ x @ y @ z)
```

If, given `a`{.haskell} of type `x`{.haskell}, `y`{.haskell} is a
type, then `x -> y`{.haskell} is a type:

```haskell
  FL : (WellTyped (x @ a) -> WellTyped (cL @ y))
       -> WellTyped (cL @ x)
       -> WellTyped (cL @ (cF @ x @ y))
```

These are non-dependent functions though. To get a dependent
type system with combinators requires the impressive 'IG'
system:

If `z`{.haskell} has an input type `x`{.haskell} and an output type
given by the function `y`{.haskell}, and `v`{.haskell} has type
`x`{.haskell}, then `zv`{.haskell} has type `yv`{.haskell}:

```haskell
  Ge : WellTyped (cG @ x @ y @ z)
       -> WellTyped (x @ v)
       -> WellTyped (y @ v @ (z @ v))
```

If, given an `a`{.haskell} of type `x`{.haskell}, `za`{.haskell} has
type `ya`{.haskell}, and `x`{.haskell} is a type, then `z`{.haskell}
is a dependent function:

```haskell
  Gi : (WellTyped (x @ a) -> WellTyped (y @ a @ (z @ a))
       -> WellTyped (cL @ x)
       -> WellTyped (cG @ x @ y @ z)
```

If, given an `a`{.haskell} of type `x`{.haskell}, `ya`{.haskell} is
a type, and `x`{.haskell} is a type, then the dependent function of
`x`{.haskell} and `y`{.haskell} is a type:

```haskell
  GL : (WellTyped (x @ a) -> WellTyped (cL @ (y @ a)))
       -> WellTyped (cL @ x)
       -> WellTyped (cL @ (cG @ x @ y))
```

These simple implementations of four typed combinatory logic
systems have been interesting me for the past week or so. A
current limitation is that datatypes must be defined as axioms.
I'm trying to overcome that with a simple porting of GADTs to
combinatory forms. It's looking promising so far :)
