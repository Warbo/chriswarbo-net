---
title: Perhaps, perhaps, perhaps
---
For those of us exploring the wild, ancient frontiers of the land of "Enterprise
Ready", there's a dangerous practice often found hiding deep within the tangled,
conditional branches of the unbalanced trees. Built by ancient civilisations of
maintainers, using primitive tools and manual labour in the traditional ritual
of accumulation and obfuscation, you can find the "pyramids of doom":

```javascript
a();
if (success(a)) {
    b();
    if (success(b)) {
        c();
        if (success(c)) {
            d();
            if (success(d)) {
                e();
                if (success(e)) {
                    f();
                    if (success(f)) {
                        g();
                        if (success(g)) {
                            h();
                            if (success(h)) {
                                i();
                                if (success(i)) {
                                    j();
                                    if (success(j)) {
                                        k();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
```

This post will show how you can conquer these monstrous edifices, which don't
crumble under the usual barrage of a refactor-hammer.

The key to tackling these hulks is to approach the refactoring problem from both
ends: to borrow a metaphor from Alan Kay, you can't replace the pyramids without
first discovering the arch.

In this case our 'arch', the elegant structure we would like to see, would look
something like this:

```javascript
a();
b();
c();
d();
e();
f();
g();
h();
i();
j();
k();
```

Now we know what we want it to look like, and what we **don't** want it to
look like, we can approach our refactoring.

The first thing we can do is to reduce the code down into the form that the
lexer will see, as this removes the artificial part of the pyramid, leaving just
its operational essense:

```javascript
a(); if (success(a)) { b(); if (success(b)) { c(); if (success(c)) { d(); if (success(d)) { e(); if (success(e)) { f(); if (success(f)) { g(); if (success(g)) { h(); if (success(h)) { i(); if (success(i)) { j(); if (success(j)) { k(); } } } } } } } } } }
```

There is a clear pattern here which we can exploit; each section of the code has
a recursive form like:

```javascript
x(); if (success(x)) { y }
```

Let's refactor this into a function, which I'll call `if_ok`{.haskell}:

```haskell
if_ok x y = x(); if (success(x)) { y }
```

With this construct in hand, we can tear down the pyramid a little more:

```javascript
if_ok(a, if_ok(b, if_ok(c, if_ok(d, if_ok(e, if_ok(f, if_ok(g, if_ok(h, if_ok(i, if_ok(j, k))))))))))
```

So far so good.

Now let's approach from the opposite direction. Let's take the same flattening
we did for the pyramid and apply it to our "arch":

```javascript
a() ; b() ; c() ; d() ; e() ; f() ; g() ; h() ; i() ; j() ; k();
```

How can we make this look like our flattened pyramid? The most conspicuous
difference is the collection of semicolons `;`  scattered throughout the
line. What do they do? The semicolon can be described as a sequencing
operator. `x ; y` means "run x then run y". We can hide the operator by
encapsulating it in a function:

```haskell
and_then x y = x() ; y()
```

This turns our arches into the following (ignoring the final semicolon, which we
could include if we used a "no-op"):

```javascript
and_then(a, and_then(b, and_then(c, and_then(d, and_then(e, and_then(f, and_then(g, and_then(g, and_then(h, and_then(i, and_then(j, k)))))))))))
```

This looks remarkably like our collapsed pyramid; in fact, the only difference
is the particular function we've used! We've found that the pyramid and the
arch have an identical structure, despite the pyramid looking horribly cryptic.

What should we do now? Well, we've brought our two spans to meet in the middle,
so we may as well finish the job and make the original pyramid code look like
our arch code. The way we do this is simple, by redefining the semicolon:

```haskell
x ; y = if_ok(x, y)
```

Now we can write pretty-much our original arch code and it will behave like the
pyramid, since we've encapsulated all of the repetitive boilerplate in a nice
little operator:

```javascript
a ; b ; c ; d ; e ; f ; g ; h ; i ; j ; k
```

You may be wondering why the original arch code called each function, ie. `a()`{.javascript}
rather than `a`; that simply depends on the language you're using. Many
languages are call-by-value, meaning that the arguments to a function/operator
are evaluated before the function/operator itself. This is bad news for if
branches, since the entire point is that they're conditional; they shouldn't run
every time!

If you're performing pure computation, then there's not much to worry about
except possibly a performance hit if you've got a naïve compiler. If you're
using a language with side-effects then you're in trouble.

If you're using a language with call-by-name or call-by-need then you're safer,
since the if conditions will short-circuit as expected. Of course, you could be
doing pure computations in a call-by-need language, in which case you're
probably using Haskell ;)

You may find it strange to overload the semicolon operator, but it's an
incredibly powerful technique. You may think it's dangerous to mix and match
definitions of such elementary operations as sequencing, but that probably means
that your type system isn't helping you enough.

With compile-time guarantees available, we can define all sorts of different
semicolons and use them without fear of getting ourselves confused. The trick is
that semicolon takes two arguments, and both have to be of the same type. These
types are known as "applicative functors" and examples include List (builds up a
tail-recursive list of values), Maybe (computations, any of which may fail and
cause the whole computation to fail) and IO (the usual 'perform actions and
receive responses' semicolon). In our example, the Pyramid of Doom is actually a
Maybe in disguise.

Some more examples of this can be found at [the Haskell wiki] [1], [Wikipedia] [2]
and on [various blogs] [3].

[1]: http://www.haskell.org/haskellwiki/Applicative_functor
[2]: http://en.wikipedia.org/wiki/Monad_(functional_programming)
[3]: http://osteele.com/archives/2007/12/cheap-monads

(Monads are a generalisation of Applicative Functors, which use composition to
send values from earlier arguments inside later ones)
