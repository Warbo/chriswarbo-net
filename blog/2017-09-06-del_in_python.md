---
title: del in Python
---

In a language like Scheme we can write an expression like this:

``` scheme
(+ 5 (let ((x 7)) (+ x x)))
```

The `let` expression defines a variable `x` which is only in scope for the
`(+ x x)` sub-expression; that binding of `x` can't be used outside that
sub-expression. In this case, the expression will evaluate like this:

```scheme
(+ 5 (let ((x 7)) (+ x x)))

;; Substitute in the value of `x`
(+ 5 (let () (+ 7 7)))

;; Unwrap the redundant `let`
(+ 5 (+ 7 7))

;; Perform the inner addition
(+ 5 14)

;; Perform the outer addition
19
```

By using a `let` expression we can be sure that (a) our expression is using the
correct value of `x` that we intended and (b) we're not breaking the value of
`x` in any *other* expressions.

Consider the following:

```scheme
(+ x (let ((x 7)) (+ x x)))
```

Now we have two *different* variables, which are *both* called `x`. Again,
evaluation still ensures (a) and (b):

```scheme
(+ x (let ((x 7)) (+ x x)))

;; Substitute in the value of the "inner" `x`
(+ x (let () (+ 7 7)))

;; Unwrap the redundant `let`
(+ x (+ 7 7))

;; Perform the inner addition
(+ x 14)
```

By using `let`, we've avoided breaking the other usage of `x`; our "inner" `x`
has disappeared, since its job is done.

Python doesn't have `let` to delimit variable scopes like this (at least, in
early versions; it may get added!). Instead, Python's scopes are tied to other
language constructs, like function bodies, loop bodies, etc.

There are ways to
[fake `let` using `with`](https://nvbn.github.io/2014/09/25/let-statement-in-python/),
although that seems to use
[dynamic scope](https://en.wikipedia.org/wiki/Dynamic_scope) rather than
"proper"
[lexical scope](https://en.wikipedia.org/wiki/Scope_(computer_science)#Lexical_scoping),
and keep in mind that Python was around for a while before `with` was added.

We can actually emulate a proper lexically-scoped `let` in Python by using
"one-shot" functions (this is actually quite common in Javascript, at least
before `let` got added to that language):

```python
x + (lambda x=7: x + x)()
```

Here the `x` argument of `lambda` is delimited to the body of that function,
which we call immediately.

This is pretty horrible to write because Python has strict rules regarding
function definitions, e.g. `lambda` can only contain one expression; whilst
`def` must name the function, and is a statement rather than an expression.

To alleviate this, we can usually avoid *opening* a new scope by using an
*assignment statement* like `x = 7`{.python}. This isn't a drop-in replacement
though, since Python allows statements to contain expressions, but *doesn't*
allow expressions to contain statements. Hence we *can't* say:

```python
x + (x = 7; x + x)
```

Instead, we have to pull the statement out of the expression:

```python
x = 7
x + (x + x)
```

This rearrangement actually changes the semantics of the code though, since it's
now equivalent to `(lambda x=7: x + (x + x))()`{.python} rather than
`x + (lambda x=7: x + x)()`{.python}

To recover the original meaning, we have to calculate each part of the
expression separately (basically re-inventing Python's order of execution),
introduce fresh names to keep the intermediate results separate (re-inventing
Python's scoping), and so on. There's no way to write this *in general*, since
it depends on what the existing variable bindings are.

In any case, the result will still not be equivalent to a "real" `let`, or a
"one-shot" `lambda`, since all of our intermediate variables will still be
in-scope.

To avoid that, we can use the `del` keyword to get rid of them.

Hence if we're writing some Python code which we'd like to simplify by splitting
up the scope, but we *don't* want to introduce new functions, etc. (since that
would *complicate* the code), then it can often be enough to introduce fresh
variables with assignments, use them for whatever *would have* gone into a
scope's body, then use `del` to "clean them up", so their presence doesn't
complicate our understanding of the subsequent code.
