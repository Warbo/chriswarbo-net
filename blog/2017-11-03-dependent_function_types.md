---
title: Dependent Function Types
---

Dependent function types allow the *return type* of a function to refer to the
*argument value* of the function. The classic example is "vectors", which are
like lists but have a statically-checked length. Here's an example function
involving vectors:

    duplicate : (t : Type) -> t -> (n : Nat) -> (Vector n t)

We read `a : b` as "`a` has type `b`", or "`a` is a `b`", so this type signature
says that `duplicate` is a function of 3 arguments: the `a -> b` notation means
a function from argument type `a` to return type `b`, and multiple "arrows" like
`a -> b -> c` can either be interpreted as:

> a function taking an `a` and returning a `b -> c`

or ([equivalently](https://wiki.haskell.org/Currying)):

> a function taking an `a` and a `b` and returning a `c`

Argument types are written in one of two ways: if they're written like `a` (for
example `Int` or `String`), then the type of the argument is `a`; if they're
written `a : b` (for example `age : Int`) then the argument type is `b`, but
later types can refer to the *value* as `a`. Note that this is overloading the
`a : b` notation for type annotations. I think of annotations like
`duplicate : ...` as "constructing" (introducing) a type-annotated value: given
a name and a type, we get a typed value; inversely, when we have a typed value,
like a function argument, we can "destruct" (eliminate, or pattern-match) it to
get a name and a type. This might be just a notational pun, but it fits nicely
into the existing concepts of constructing and destructing data.

So this signature says that the first argument of `duplicate` is called `t`, and
it is a `Type`. The second argument has type `t`; this means the *type* of the
second argument *depends* on the *value* of the first argument: if we call
`duplicate` with `Bool : Type` as the first argument, then the second argument
must be a `Bool`.

The third argument is called `n`, and has type `Nat` (natural numbers: i.e. the
positive integers including zero). The return type is `Vector n t`, i.e. a list
of length `n` containing elements of type `t`.

Note that we're referring to argument values by name, but we don't know what
*particular* value they will be (i.e. which number `n` will be, or which type
`t` will be). We can think of `a : b` as being "for *all* `a` of type `b`".

If we think about functions returning lists, like `map` or `reverse`, we can
"cheat" by having them always return an empty list; such functions satisfy their
type signature (they return a list), but don't behave in the way we want.

In contrast, the "for all" nature of dependent function types can be used to
prevent this sort of "cheating". The `duplicate` function can't just return an
empty vector, since it must return a vector of length `n`, and the
implementation must work *for all* values of `n`. We're *forced* to write a
function which constructs a vector of the correct length: empty when `n` is
zero, or non-empty otherwise.

To construct a non-empty vector, we need to put a value inside it. What value
can we use? We can't "cheat" by using, say, the value `"hello"`, since that
would give us a vector containing `String`, whilst our return type forces us to
make a vector containing `t`. Whatever our function does, it must work *for all*
values of `t` (`String`, `Bool`, `Int`, `Vector 5 (Int -> Bool)`, etc.).

Since we don't know what `t` will be, we can't "invent" a value of the right
type. The only thing we can do is use an *existing* value which will always have
type `t`. The only way something can have type `t` is if it appears in our type
signature, since that's the scope of the name `t`. The only thing in our type
signature which has type `t` is the second argument. Hence we *must* use the
second argument as the elements of our vector (if it's non-empty).

One implementation which satisfies this type is the following (assuming that
`Nat` is encoded as
[Peano numerals](https://en.wikipedia.org/wiki/Successor_function)):

    duplicate t x n = case n of
                           zero   -> vectorNil t
                           succ m -> vectorCons t x (duplicate t x m)

This isn't the only possible implementation, for example we might choose to send
the recursive call through a `vectorReverse` function (although it would be
pointless), but such differences cannot alter the input/output behaviour of the
function.

Of course, we don't have to use dependent function types to restrict *all* of a
function's behaviour. If we think back to the `map` example, we take a function
of type `a -> b`, a `List a` and must return a `List b`: the easiest thing to do
is return an empty list, which satisfies the type but isn't what we want.

If we instead make a `vectorMap` function, where `List` is replaced with
`Vector n`, then that "cheat" no longer works (since an empty vector doesn't
always have length `n`). We *could* still mess around with the contents, like
reversing it or doing some other permutation, but that's *harder* than just
doing the right thing. The path of least resistance is to write a correct
implementation!
