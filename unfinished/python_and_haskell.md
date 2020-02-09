---
title: Bottom, Null, Unit and Void
---

When programming (or theorem-proving), it's important to distinguish between the
*empty type* and the *unit type*. In (type) theory there are *no* values of the
empty type, and there is *one* value of the unit type. In a language without
"bottom" (AKA `_|_`{.haskell}), like Agda, we might define them something like
this:

```haskell
data Empty : Type

data Unit : Type where
    unit : Unit
```

This says that `Empty`{.haskell} is a `Type`{.haskell}, that `Unit`{.haskell} is
a `Type`{.haskell}, that `unit`{.haskell} is a `Unit`{.haskell}. Since we've
given no constructors (or "introduction forms") for `Empty`{.haskell}, there's
no way to make one. Likewise we can write function types like
`Int -> Empty`{.haskell} which have no values (since there's no way for any
function to return a value of type `Empty`{.haskell}), we can write tuple types
like `Pair Int Empty`{.haskell} that have no values, etc. We can also write
function types like `Empty -> Int`{.haskell} which *do* have values (e.g.
`const 42`{.haskell}), but which can never be called (since there are no
`Empty`{.haskell} values to give as an argument).

Incidentally, the fact that we can have types with no values is one of the
reasons we can't have a value with type `forall a. a`{.haskell} (in Haskell) or
`(a : Type) -> a`{.haskell} (in Agda): they claim to return a value of any type,
but we might ask for an `Empty`{.haskell}.

The unit type is trivial, literally. Since there's only one value of this type
(`unit`{.haskell}), whenever we know that a value will have type
`Unit`{.haskell} we can infer that the value *must* be `unit`{.haskell}, and
hence:

 - If it's a return value, we can always optimise the function to simply
   `unit`{.haskell}
 - If it's an argument, it has no effect on the return value (there's nothing to
   branch on).

In this sense, the unit type contains 0 bits of information (compared to
`Bool`{.haskell} which contains 1 bit).

As an aside, `Either`{.haskell} is called a "sum type" because it contains all
the values from its first argument *plus* those of its second:
`Either Unit Unit`{.haskell} is equivalent to `Bool`{.haskell} (with values
`Left unit`{.haskell} and `Right unit`{.haskell}). Pairs are called "product
types" since they contain every combination of their first and second arguments,
which is the number of values in each *multiplied* together. In this sense
`Empty`{.haskell} acts like zero: the type `Either Empty a`{.haskell} doesn't
contain any `Left`{.haskell} values, so it's equivalent to `a`{.haskell}, just
like `0 + a = a` for numbers. Likewise `Pair Empty a`{.haskell} contains no
values, since there's nothing to put in the first position, just like
`0 * a = 0` for numbers. `Either Unit a`{.haskell} acts like `1 + a`, since we
have all of the values of `a`{.haskell} (wrapped in `Right`{.haskell}) plus the
extra value `Left unit`{.haskell}; note that this is also the same as
`Maybe a`{.haskell}, where `Right`{.haskell} acts like `Just`{.haskell} and
`Left unit`{.haskell} acts like `Nothing`. `Pair Unit a`{.haskell} is the same
as `a`{.haskell} since each `a`{.haskell} value can only be paired with one
thing, and we know that thing is always `unit`{.haskell}; this is just like
`1 * a = a` with numbers.

If we want to compare these things in "real" languages, it can get confusing,
especially since a lot of the terminology is overloaded. In Haskell the type
`Unit`{.haskell} is written `()`{.haskell} and the value `unit`{.haskell} is
*also* written `()`{.haskell}.

In Haskell the type `Empty`{.haskell} is called
[`Void`{.haskell}](
https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Void.html),
but that is *not* the same as `void`{.java} in languages like Java! In
particular, remember that we cannot have a function which returns
`Empty`{.haskell}, so `void`{.java} being `Empty`{.haskell} would mean we could
not implement methods like `public static void putStrLn(String s)`{.java}. Such
methods certainly *do* exist, since they're all over the place in Java, but what
happens if we call them? They will run, and return a value back to us (hence it
can't be `Empty`{.haskell}). What *is* the value we get back? We know, without
even running the method, that we'll get back `null`{.java}. That's just like the
`Unit`{.haskell} type (where we know the value will be `unit`{.haskell} without
having to run the code). Hence Java's `void`{.java} acts like `Unit`{.haskell},
and `null`{.java} acts like `unit`{.haskell}.

If we follow a similar line of reasoning in Python, we find that `None`{.python}
acts like `unit`{.haskell} (or Java's `null`{.java}) and hence
`NoneType`{.python} is like `Unit`{.haskell} (or Java's `void`{.java}). AFAIK
Python doesn't have anything which acts like `Empty`{.haskell} (Haskell's
`Void`{.haskell}) since, lacking any values, `Empty`{.haskell} is only useful
for type-level calculations (of the sort which get erased during compilation),
which Python doesn't tend to do.

That just leaves "bottom". There are a couple of ways to think about it: we can
be ["fast and
loose"](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/fast+loose.pdf)
where we ignore bottom as a nuisance, which mostly works since bottom isn't a
"proper" value: in particular, we can't branch on it; hence any time we do a
calculation involving bottoms which results in a "proper" value, we know that
the bottoms were irrelevant to the result, and hence can be ignored. Any time a
bottom *is* relevant, we have to abandon our nice, logical purity in any case,
since catching them requires `IO`{.haskell}, so why bother complicating our pure
logic by trying to include them?

Alternatively we can treat bottom as an extra value of every type, in a similar
way to `null`{.java} inhabiting every type in Java. From this perspective
Haskell's `Void`{.haskell} type *does* contain a value (bottom), and the
`()`{.haskell} type contains *two* values (`()`{.haskell} and bottom). In this
sense we might think of Java's `void`{.java} corresponding to Haskell's
`Void`{.haskell}, but I find this line of thinking difficult to justify. In
particular, we *can* branch on `null`{.java} in Java (in fact we *should*
be doing such "null checks" all the time!), whereas (pure) Haskell doesn't even
let us tell whether we have `()`{.haskell} or bottom.

As a final thought, whenever comparing dynamic and static languages, it's
important to not conflate different concepts which just-so-happen to have
similar names. In particular I find it useful to think of dynamically typed
languages as being
["uni-typed"](
https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages):
that is, every value has the same type, which we might write in Haskell as a sum
like
`data DynamicValue = S String | I Int | L List | O Object | E Exception | ...`{.haskell}.
Python is actually even simpler than this, since "everything is an object" (not
quite as much as in Smalltalk, but certainly more than e.g. Java), so Python
values are more like a pair of a class (which itself is a Python value) and a
collection of instance properties.

This is important because "dynamic types", like in Python, aren't directly
comparable to "static types" like those of Haskell; they're more comparable to
"tags" (e.g. constructors). In particular, think about a Python function
branching based on its argument's "dynamic type"; this is the same as a Haskell
function branching on its argument's *constructor*. What does this tell us about
"bottom" in Python? From this perspective, there isn't one (at least not
reified; an infinite loop might be comparable, but it's not something we can
e.g. assign to a variable). Python's `None`{.python} is just a normal value like
any other, in the same way that Haskell's `Nothing`{.haskell} is a normal value
(we might sometimes use it to *stand for* an error, but that's not inherent to
the system); likewise Python's exceptions are *also* normal values (we can
assign them to variables, return them from functions, etc.); the idea of
"throwing and catching" (or `raise`{.python}/`except`{.python} for Python) is
actually a perfectly normal method of control flow (it's actually a limited form
of [delimited continuation](
https://en.wikipedia.org/wiki/Delimited_continuation)), and this is orthogonal
to error representation and handling.

This makes raising an exception in Python *very* different to triggering a
bottom in Haskell, since Haskell provides no way to branch on a bottom (or
whether we even have one). In Python we can raise a value (with the exception
"tag") as an alternative to using `return`{.python}, and we can catch those
values, inspect their tag and value, etc. to determine what our overall return
value will be, with none of this being visible by the caller. To do anything
like that in Haskell we need some "proper" data to inspect and branch on, hence
why I say `None`{.python}, exceptions, etc. are just normal values (even though
they're used to represent errors, and we treat them specially because of that;
but the same could be said about `Either String a`{.haskell} values in Haskell,
for example). Consider that in Haskell the only way to even check if a bottom
exists is to use `IO`{.haskell}, but the idea behind `IO`{.haskell} is that it's
like [`State RealWorld`](
https://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Prim.html#g:14),
i.e. every `IO a`{.haskell} value is a *pair* of `(RealWorld, a)`{.haskell}, so
conceptually we never "actually" see a bottom; it's more like triggering a
bottom changes the `RealWorld`{.haskell}, and we're branching on that.
