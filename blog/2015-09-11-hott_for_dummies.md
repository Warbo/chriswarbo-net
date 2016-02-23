---
title: HoTT for Dummies
---

Based on [a Hacker News thread](https://news.ycombinator.com/item?id=10184324), about [a talk][slides] by [Thorsten Altenkirch](http://www.cs.nott.ac.uk/~txa/), I ended up writing a rather long explanation of some Type Theory and Homotopy Type Theory, which I thought I might as well reproduce here:

Let's say you're creating a new programming language. You think it's a good idea to use static types, but which ones? Well, everyone uses booleans, so let's include them:

```haskell
True : Boolean
False : Boolean
```

(`Foo : Bar`{.haskell} means "`Foo`{.haskell} has type `Bar`{.haskell}", or "`Foo`{.haskell} is a `Bar`{.haskell}"). OK, that seemed pretty easy. But wait, you've not given a type to `Boolean`{.haskell}. For the sake of completeness:

```haskell
Boolean : Type
```

Uh oh, now you need to give a type to `Type`{.haskell}. What should it be? It turns out (via [Girard's paradox](http://mathoverflow.net/questions/18089/what-is-the-manner-of-inconsistency-of-girards-paradox-in-martin-lof-type-theor)) that simply saying `Type : Type`{.haskell} would make things inconsistent, ie. we would be able to trick the compiler into accepting incorrect programs.

Instead, we use a series of "levels":

```haskell
Boolean : Type 0

Type n : Type (n+1)
```

So far so good. Now let's say we want function types, for example:

```haskell
identity : Boolean -> Boolean
identity x = x

not : Boolean -> Boolean
not True  = False
not False = True
```

So what's this `->`{.haskell} thing? We can think of it as a type-level operator: it takes two types and returns a function type. In the syntax of [natural deduction](https://en.wikipedia.org/wiki/Natural_deduction), we can say:

```haskell
a : Type n         b : Type m
-----------------------------
 a -> b : Type (1 + max n m)
```

ie. given `a`{.haskell} of type `Type n`{.haskell}, and `b`{.haskell} of type `Type m`{.haskell}, then `a -> b`{.haskell} has type `Type (1 + max n m)`{.haskell}. Because the type `a -> b`{.haskell} somehow 'contains' the types `a`{.haskell} and `b`{.haskell}, we need to ensure it's at a higher level than either of them, which is why we do `1 + max n m`{.haskell}.

In fact, there's no reason for the `identity`{.haskell} function to only work on `Boolean`{.haskell}s. We can replace it with an "identity function factory", which accepts a type and returns an identity function for that type:

```haskell
identity : (t : Type n) -> t -> t
identity x y = y
```

Here we've re-used the `foo : bar`{.haskell} notation: rather than just giving the type of the first argument, we've also introduced a variable `t`{.haskell} representing its value (this is known as a dependent function). Notice that the definition of `identity`{.haskell} actually ignores the type it's been given (`x`{.haskell}); the implementation doesn't care what it is, it'll just return the second argument no matter what; yet we need that argument in order to type-check. When we compile this program, we can "erase" the first argument, since it has no "computational content".

We can recover our old `identity`{.haskell} function, of type `Boolean -> Boolean`{.haskell}, by applying this new `identity`{.haskell} function to the `Boolean`{.haskell} type:

```haskell
identity Boolean : Boolean -> Boolean
```

OK, what next? Well, since we have functions, we might as well have function composition:

```haskell
compose : (a : Type x) -> (b : Type y) -> (c : Type z) -> (b -> c) -> (a -> b) -> a -> c
compose t1 t2 t3 g f x = f (g x)
```

It would also be useful to have equality. Here it is for `Boolean`{.haskell}s:

```haskell
equal : Boolean -> Boolean -> Boolean
equal True  x = x
equal False x = not x
```

However, just like the `identity`{.haskell} function, this isn't very satisfying. We'd like an "equality function factory", with this type:

```haskell
equal : (t : Type n) -> t -> t -> Boolean
```

Except, how would we ever implement such a "factory"? It was easy for `identity`{.haskell}: we just return whatever we're given. In the case of equality, we need to inspect our arguments, to see whether they're actually equal or not. We can't do this in a way which works for all types (eg. what if we allow user-defined types?).

However, there's a trick. By returning a `Boolean`{.haskell}, we're defining equality (`True`{.haskell}) *and* disequality (`False`{.haskell}). That's hard. Instead, we can ignore the disequality, and only focus on equality, using a different return type; let's call it `Equal x y`{.haskell}.

What does it mean for two things to be equal? It means that they're the same thing. In which case, we don't need both of them! Every value is equal to itself (a property known as "reflexivity"), so that's all we need!

```haskell
refl : (t : Type n) -> (x : t) -> Equal x x
```

For example, here's equality for the `Boolean`{.haskell}s:

```haskell
refl Boolean True  : Equal True  True
refl Boolean False : Equal False False
```

Note that `refl`{.haskell} isn't actually a function, it's a data constructor. You can think of a value like `refl Boolean True`{.haskell} as being a piece of data, similar to something like `pair Int String 10 'foo'`{.haskell}; it doesn't reduce to anything, it just gets passed around as-is. (These are often called "proof objects", but that's a bit arbitrary; a value like `pair Int String 10 'foo'`{.haskell} is a "proof" of "`Int`{.haskell} AND `String`{.haskell}").

If we allow computation in our types, then two different values which compute (technically: beta reduce) to the same thing are still equal by reflexivity:

```haskell
refl Boolean True : Equal True (not False)
```

Here, the `not False`{.haskell} will compute to `True`{.haskell}, and `refl`{.haskell} will type-check. Different values, eg. `False`{.haskell} and `True`{.haskell}, are never equal, since they don't reduce to the same thing. We can make our computations as complex as we like, for example:

```haskell
refl Boolean True : Equal (identity Boolean True) (compose Boolean Boolean Boolean not not True)
```

We can even have equality between functions and equality between types:

```haskell
-- "identity" is equal to "identity"
refl ((t : Type n) -> t -> t) identity : Equal identity identity

-- "Boolean" is equal to "Boolean"
refl (Type 0) Boolean : Equal Boolean Boolean
```

We can even have equalities between equalities!

```haskell
-- "refl Boolean True" is equal to "refl Boolean True"
refl (Equal True True) (refl Boolean True) (refl Boolean True) : Equal (refl Boolean True) (refl Boolean True)
```

Most of this predates Homotopy Type Theory, so what are the points being made in [the slides][slides]?

One point is to give a topological perspective for types: a type is like a space, values in the type are like points in the space. Equalities between values are paths in the space (eg. `refl Boolean True`{.haskell} is a trivial path from the point `True`{.haskell} to itself, in the `Boolean`{.haskell} space). Interestingly, equalities between equalities are homotopies (smooth transformations between paths).

One question we might ask is whether all equality values are the same; ie. are they all just `refl`{.haskell}? That's known as the "Uniqueness of Identity Proofs" (UIP), and it's an assumption that many people have been making for decades. However, if we think of equalities as paths through a space, then UIP says that all those paths can be transformed into each other. Yet that's not the case if the space contains a hole! Consider two paths going from a point `X` back to itself; if one of those paths loops around a hole, and the other doesn't, then there's no way to smoothly transform between the two (without "cutting and sticking"):

```{pipe="cat > hole.dit"}
/----------------------\
|                      |
|  /----\              |
|  |    :              |
|  |    V              |
|  \----X<----------\  |
|       |           |  |
|       :   /-----\ |  |
|       |   |     | |  |
|       |   \-----/ |  |
|       \-----------/  |
|                      |
\----------------------/
```

```{.unwrap pipe="sh | pandoc -t json"}
nix-shell -p ditaa --run "ditaa hole.dit hole.png" >> /dev/stderr
./root/static/file2img.sh "Type containing a hole" < hole.png
```

The topological perspective also gives us some intuition about the "levels" of types: `Type 0`{.haskell} contains spaces with distinct points, eg. `Boolean`{.haskell} containing `True`{.haskell} and `False`{.haskell}. These are essentially sets, from Set Theory. Although HoTT doesn't assume UIP for all types, those which *do* just-so-happen to "collapse" down to one value (ie. there are equalities between every point) actually occupy a level *below* sets; ie. they end up at `Type -1`{.haskell} (there's no significance to the negative number; it's just a historical accident caused by definitions like `Boolean : Type 0`{.haskell}). Likewise, those which add more structure occupy higher levels.

One important question is how function equality behaves. It's useful to have equality for, say, `Boolean`{.haskell}s, since we can compute their value and check whether they're the same. Functions are trickier: we have *intensional* equality (eg. `Equal identity identity`{.haskell}) but we'd like *extensional* equality (eg. that `identity`{.haskell} and `not . not`{.haskell} are equal, ie. `Equal (identity Boolean) (compose Boolean Boolean Boolean not not)`{.haskell}). This is tricky. If we assume extensionality as an axiom (ie. we assume a built-in function of type `((x : t) -> Equal (f x) (g x)) -> Equal f g`{.haskell}), like in NuPRL, then we lose the ability to execute our programs (axioms are basically "built in primitives"; we don't know how to implement such an extensionality primitive). Observational Type Theory gets us most of the way there, but relies on UIP, which would collapse all of the higher-level structure in HoTT.

Univalence is also a nice feature of HoTT. It's incompatible with UIP, but allows us to reason "up to isomorphism". For example, in set theory the sets `{foo, bar}` and `{x, y}` are isomorphic: eg. we can switch `foo` with `x` and `bar` with `y`. However, results concerning `{foo, bar}` may be invalid for `{x, y}`, since set theory lets us say things like "is `foo` a member of `S`?", which is `true` when `S = {foo, bar}` but `false` when `S = {x, y}`. HoTT doesn't let us say things like "is `foo` a member of type `T`?", hence we don't get these kind of "abstraction leaks", so our programs and proofs can be automatically lifted from one representation to another. For example, if you define a fast, distributed datastructure which is isomorphic to a linked-list, then you can automatically lift every linked-list program to your new datastructure using univalence. Unfortunately, we haven't figured out how to implement univalence yet (but many people think it's possible).

Some of the results listed at the end of [the slides][slides] concern the use of HoTT to prove topological results, eg. that the integers are isomorphic to paths around a hole (you can go around the hole any number of times, clockwise or anticlockwise).

[slides]: http://www.cs.nott.ac.uk/~txa/talks/edinburgh-13.pdf
