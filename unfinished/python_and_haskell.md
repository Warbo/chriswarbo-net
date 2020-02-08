> I am really curious as to whether Python’s NoneType would map to Haskell’s
> bottom in that regards.

It's important to distinguish between the *empty type* and the *unit type*. In
(type) theory there are *no* values of the empty type, and there is *one* value
of the unit type. In a language without `_|_`, like Agda, we might define them
something like this:

```
data Empty : Type

data Unit : Type where
    unit : Unit
```

This says that `Empty` is a `Type`, that `Unit` is a `Type`, that `unit` is a
`Unit`. Since we've given no contructors (or "introduction forms") for `Empty`,
there's no way to make one. Likewise we can write function types like `Int ->
Empty` which have no values (since there's no way for any function to return a
value of type `Empty`), we can write tuple types like `Pair Int Empty` that have
no values, etc. We can also write function types like `Empty -> Int` which *do*
have values (e.g. `const 42`), but which can never be called (since there are no
`Empty` values to give as an argument). Incidentally, the fact that we can have
types with no values is one of the reasons we can't have a value with type
`forall a. a` (in Haskell) or `(a : Type) -> a` (in Agda): they claim to return
a value of any type, but we might ask for an `Empty`.

The unit type is trivial, literally. Since there's only one value of this type
(`unit`), whenever we know that a value will have type `Unit` we can infer that
the value *must* be `unit`, and hence (a) if it's a return value, we can always
optimise the function to simply `unit` and (b) if it's an argument, it has no
effect on the return value (there's nothing to branch on). In this sense, the
unit type contains 0 bits of information (compared to `Bool` which contains 1
bit).

As an aside, `Either` is called a "sum type" because it contains all the values
from its first argument *plus* those of its second: `Either Unit Unit` is
equivalent to `Bool` (with values `Left unit` and `Right unit`). Pairs are
called "product types" since they contain every combination of their first
argument and second arguments, which is the number of values in each
*multiplied* together. In this sense `Empty` acts like zero: the type `Either
Zero a` doesn't contain any `Left` values, so it's equivalent to `a`, just like
`0 + a = a` for numbers. Likewise `Pair Empty a` contains no values, since
there's nothing to put in the first position, just like `0 * a = 0` for
numbers. `Either Unit a` acts like `1 + a`, since we have all of the values of
`a` (wrapped in `Right`) plus the extra value `Left unit`; note that this is
also the same as `Maybe a`, where `Right` acts like `Just` and `Left unit` acts
like `Nothing`. `Pair Unit a` is the same as `a` since each `a` value can only
be paired with one thing, and we know that thing is always `unit`; this is just
like `1 * a = a` with numbers.

Back to your question: things get confusing when we try to apply this to "real"
languages, especially since a lot of the terminology is overloaded. In Haskell
the type `Unit` is written `()` and the value `unit` is *also* written `()`.

In Haskell the type `Empty` is called
[`Void`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Void.html),
but that is *not* the same as `void` in languages like Java! In particular,
remember that we cannot have a function which returns `Empty`, so `void` being
`Empty` would mean we could not implement methods like `public static void
putStrLn(String s)`. Such methods certainly *do* exist, since they're all over
the place in Java, but what happens if we call them? They will run, and return a
value back to us (hence it can't be `Empty`). What *is* the value we get back?
We know, without even running the method, that we'll get back `null`. That's
just like the `Unit` type (where we know the value will be `unit` without having
to run the code). Hence Java's `void` acts like `Unit`, and `null` acts like
`unit`.

If we follow a similar line of reasoning in Python, we find that `None` acts
like `unit` (or Java's `null`) and hence `NoneType` is like `Unit` (or Java's
`void`). AFAIK Python doesn't have anything which acts like `Empty` (Haskell's
`Void`) since, lacking any values, `Empty` is only useful for type-level
calculations (of the sort which get erased during compilation), which Python
doesn't tend to do.

That just leaves "bottom". There are a couple of ways to think about it: we can
be ["fast and
loose"](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/fast+loose.pdf)
where we ignore bottom as a nuisance, which mostly works since bottom isn't a
"proper" value: in particular, we can't branch on it; hence any time we do a
calculation involving bottoms which results in a "proper" value, we know that
the bottoms were irrelevant to the result, and hence can be ignored. Any time a
bottom *is* relevant, we have to abandon our nice, logical purity in any case,
since catching them requires `IO`, so why bother complicating our pure logic by
trying to include them?

Alternatively we can treat bottom as an extra value of every type, in a similar
way to `null` inhabiting every type in Java. From this perspective Haskell's
`Void` type *does* contain a value (bottom), and the `()` type contains *two*
values (`()` and bottom). In this sense we might think of Java's `void`
corresponding to Haskell's `Void`, but I find this line of thinking difficult to
justify. In particular, we *can* branch on `null` in Java (in fact we *should*
be doing such "null checks" all the time!), whereas (pure) Haskell doesn't even
let us tell whether we have `()` or bottom.

As a final thought, whenever comparing dynamic and static languages, it's
important to not conflate different concepts which just-so-happen to have
similar names. In particular I find it useful to think of dynamically typed
languages as being
["uni-typed"](https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages):
that is, every value has the same type, which we might write in Haskell as a sum
like `data DynamicValue = S String | I Int | L List | O Object | E Exception |
...`. Python is actually even simpler than this, since "everything is an object"
(not quite as much as in Smalltalk, but certainly more than e.g. Java), so
Python values are more like a pair of a class (which itself is a Python value)
and a collection of instance properties.

This is important because "dynamic types", like in Python, aren't directly
comparable to "static types" like those of Haskell; they're more comparable to
"tags" (e.g. constructors). In particular, think about a Python function
branching based on its argument's "dynamic type"; this is the same as a Haskell
function branching on its argument's *constructor*. What does this tell us about
"bottom" in Python? From this perspective, there isn't one (at least not
reified; an infinite loop might be comparable, but it's not something we can
e.g. assign to a variable). Python's `None` is just a normal value like any
other, in the same way that Haskell's `Nothing` is a normal value (we might
sometimes use it to *stand for* an error, but that's not inherent to the
system); likewise Python's exceptions are *also* normal values (we can assign
them to variables, return them from functions, etc.); the idea of "throwing and
catching" (or `raise`/`except` for Python) is actually a perfectly normal method
of control flow (it's actually a limited form of [delimited
continuation](https://en.wikipedia.org/wiki/Delimited_continuation)), and this
is orthogonal to error representation and handling.

This makes raising an exception in Python *very* different to triggering a
bottom in Haskell, since Haskell provides no way to branch on a bottom (or
whether we even have one). In Python we can raise a value (with the exception
"tag") as an alternative to using `return`, and we can catch those values,
inspect their tag and value, etc. to determine what our overall return value
will be, with none of this being visible by the caller. To do anything like that
in Haskell we need some "proper" data to inspect and branch on, hence why I say
`None`, exceptions, etc. are just normal values (even though they're used to
represent errors, and we treat them specially because of that; but the same
could be said about `Either String` values in Haskell, for example). Consider
that in Haskell the only way to even check if a bottom exists is to use `IO`,
but the idea behind `IO` is that it's like [`State
RealWorld`](https://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Prim.html#g:14),
i.e. every `IO a` value is a *pair* of `(RealWorld, a)`, so conceptually we
never "actually" see a bottom; it's more like triggering a bottom changes the
`RealWorld`, and we're branching on that.
