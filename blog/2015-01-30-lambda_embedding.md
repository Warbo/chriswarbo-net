---
title: Embedding Lambda Calculus
---

Functional programming makes heavy use of *embedded domain-specific languages* (EDSLs). These make our programming language (the *host language*) *behave like* another language (the *embedded language*). This is similar to the way Object-Oriented libraries often choose class, property and method names which 'chain together' nicely, eg. `Mailer.send(Order.invoiceFor(Session.user))`{.java}; except functional languages aren't limited to `arg1.method(arg2, arg3, ...)`{.java} syntax. We can use `function arg1 arg2 arg3 ...`, and use functions as arguments (higher-order functions).

Due to this interest in embedding, the desire to use purely-functional languages, and the desire for minimalism/simplicity, we functional programmers spend an unhealthy amount of time trying to embed various forms of lambda calculus inside our functional languages (which themselves are usually some form of lambda calculus).

There are a few ways we can do this:

## First-Order Representations ##

A *first-order* representation uses *data* to represent lambda calculus *programs*. An example would be:

```haskell
data FirstOrderTerm = Var String
                    | Lam String FirstOrderTerm
                    | App FirstOrderTerm FirstOrderTerm
```

This represents the three syntactic forms of lambda calculus terms directly:

 - `Var`{.haskell}iables with a `String`{.haskell} for a name
 - `Lam`{.haskell}bda abstractions with a `String`{.haskell} for the argument name and a `FirstOrderTerm`{.haskell} for the body
 - `App`{.haskell}lication of one `FirstOrderTerm`{.haskell} to another

This is fine for *human* use, but it's no good for a machine; the space of `String`{.haskell}s is too large and complicated for software to understand. It's very unlikely that two arbitrary strings will match, so therefore unlikely that an auto-generated variable will match the argument of an auto-generated binder. We're much better off *numbering* our arguments, which limits our options and increases the chances of a match. If we use *de Bruijn indices*, which I've blogged about before, the numbers have a *consistent meaning* in the meta-language (although they're often avoided by humans because they have inconsistent, or rather *context-dependent*, meanings in the object language):

```haskell
data Nat = Z | S Nat

data DeBruijnTerm = Var Nat
                  | Lam DeBruijnTerm
                  | App DeBruijnTerm DeBruijnTerm
```

## Higher-Order Representations ##

A higher-order representation uses *functions* to represent part of a lambda term. A classic implementation of *Higher Order Abstract Syntax* looks like this:

```haskell
data HigherOrderTerm = Lam (HigherOrderTerm -> HigherOrderTerm)
                     | App HigherOrderTerm HigherOrderTerm
```

There are two clear differences to note: firstly, `Lam`{.haskell} now takes a function rather than a term. We can understand this by thinking of the `Lam`{.haskell} as a 'term containing *holes*'; to "filled in" those holes, we must pass in a term, hence the use of a function type.

So what are those 'holes'? *They're the variables!* Notice that we don't need a separate `Var`{.haskell} constructor, because:

 1) Variables only make sense inside `Lam`{.haskell} terms (think back to de Bruijn indices!)
 2) Each `Lam`{.haskell} term now contains a Haskell function
 3) Haskell functions let us use Haskell's implementation of variables

Haskell variables are the "holes" in our terms!

This is a very neat trick, but it has a few problems:

 - The `Lam`{.haskell} constructor is not 'strictly positive'. This allows non-termination, and hence logical inconsistency. That's fine in Haskell, which already suffers from non-termination, but it prevents us implementing this type directly in safe languages like Coq.
 - The Haskell functions stored in `Lam`{.haskell} constructors are *opaque*; ie. we can't "look under a lambda". This prevents us performing any kind of term traversal, eg. pretty-printing, gathering information/statistics about terms, checking syntactic equality, performing rewrites/optimisations, serialising/deserialising (excluding hacks like lambda-lifting used in Cloud Haskell)

## Hybrid ##

There are hybrid solutions, like the use of "circular programming" to define first-order representations from a higher-order *interface*. For example, this will generate terms with non-conflicting variable names, in a more "natural" way than de Bruijn's "inside-out" numbering:

```haskell
data CircularTerm = Var Nat
                  | Lam Nat CircularTerm
                  | App CircularTerm CircularTerm

max :: Nat -> Nat -> Nat
max  Z       y  = y
max  x       Z  = x
max (S x) (S y) = S (max x y)

maxBoundVar :: CircularTerm -> Nat
maxBoundVar (Var _)   = Z
maxBoundVar (Lam n _) = n
maxBoundVar (App f x) = max (maxBoundVar f) (maxBoundVar x)

lam :: (CircularTerm -> CircularTerm) -> CircularTerm
lam f = let n    = S (maxBoundVar body)
            body = f (Var n)
         in Lam n body
```

Unfortunately this doesn't translate easily to total languages like Coq. The `n`{.haskell} and `body`{.haskell} variables in `lam`{.haskell} are mutually-recursive, and exploit the laziness of `maxBoundVar`{.haskell}. What's worse, this recursion will only terminate if `f`{.haskell} produces 'well-formed' terms, ie. it only uses the `App`{.haskell} and `lam`{.haskell} interfaces, rather than constructing its own `Lam`{.haskell} or `Var`{.haskell} terms directly. This causes problems since we tend to reason about values using *universal* quantifiers: ie. `forall f, SomeProperty (App (lam f) x)`{.ocaml}, which requires us to litter our code with *witnesses* to demonstrate that, indeed, our functions are safe.

## Encoding/decoding ##

I've written about Morgensen-Scott encoding before. We can use their scheme to encode any 'polynomial' datatype as a lambda calculus function. As long as we use a first-order representation, this allows us to reason "under the lambda", but it does have associated dangers.

Whenever we translate from a typed to an untyped (or *unityped*) representation, we lose information. We cannot, in general, convert *back* to a typed representation. We're forced to either wrap our decoders' return types in a *maybe*, or else provide it with a witness to demonstrate that the term is well-typed.

We can 'compose over' these requirements easily enough, eg. using functors, applicatives and monads to compose across maybes, and perhaps some kind of state monad to carry the witnesses. However, since both of these are *conservative* approaches ('guilty until proven innocent'), every *decoding* we perform will limit the space of possible programs we allow: the number of values which survive the maybe, or which have accessible proofs, is monotonically-decreasing as we add more decoding steps.

Hence it's a bad idea to convert back and forth "in the small", regardless of whether we have monads to make it convenient: if we're going to perform any conversion at all, we should convert *the entire remainder of our program* (or the *continuation*, if you prefer).

That may sound like madness: we'd be throwing away the safety of our typed language, surely? That's where powerful type systems really shine: we can type-check *the encoding process*, to guarantee that only well-typed encodings are produced, even though the encoding language is untyped. We actually do this all the time: it's exactly what a compiler does! Encoding our continuation is like *staged compilation*. Since types (not to be confused with *tags* or *classes*) only exist at compile time, we *always* end up running an untyped (/unityped) language in the end, since that's the only kind of language which *can* "run"!
