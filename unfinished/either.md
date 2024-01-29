> In languages with better ADT/pattern matching support (like Haskell or Rust)
> you'd use case/match operator, or whatever it's called.

When chaining lots of calls together it's usually a better idea to use some
combination of `map`, `app`/`product` and `flatMap`/`join` (these functions are
usually provided by interfaces called functor, applicative and monad,
respectively).

For example:

```python
def example(a):
  b = foo(a)
  if isinstance(b, Exception): return b
  c = bar(b)
  if isinstance(c, Exception): return c
  d = baz(quux(c))
  if isinstance(d, Exception): return d
  return foobar(d)
```

You're right that in Haskell/ML/Scala/etc. we would probably use
`Either[Exception, T]`{scala} (Scala has a type called `Try[T]`{scala} which is
essentially the same; its values are `Success(T)`{scala} and
`Failure(Throwable)`{scala}).

We *could* pattern-match on them, but that gets tedious quite quickly, e.g. in
Scala

```scala
def example(a: Either[Exception, Foo]): Either[Exception, FooBar] = a match {
  case  Left(e) => Left(e)
  case Right(b) => bar(b) match {
    case  Left(e) => Left(e)
    case Right(c) => bazz(quux(c)) match {
      case  Left(e) => Left(e)
      case Right(d) => foobar(d)
    }
  }
}
```

We've not really gained anything here, since the `isinstance...: return...`
lines have just become `case Left(e) => Left(e)`{scala}. Note that we don't need
pattern-matching for this; we can do the same thing using functions (a form of
[Church encoding](https://en.wikipedia.org/wiki/Church_encoding)) in any of 
these languages, including Python, like this:

```python
def example(a):
  a.fold(
    lambda e: Left(e),
    lambda b: bar(b).fold(
      lambda e: Left(e),
      lambda c: bazz(quux(c)).fold(
        lambda e: Left(e),
        lambda d: foobar(d)
      )
    )
  )
```

This assumes we have:

```python
class Either:
  pass

class Left(Either):
  def __init__(x):
    self.x = x

  def fold(f, g):
    return f(self.x)

class Right(Either):
  def __init__(y):
    self.y = y

  def fold(f, g):
    return g(self.y)
```

There's nothing particularly special about using dynamic dispatch; we could
define both parts of `fold` as a standalone function instead, like:

``` python
fold = lambda x, f, g: f(x.x) if isinstance(x, Left) else g(x.y)
```

Note that this is basically like my first example, but the `isinstance`
conditional has been turned into a function with the branches as
arguments.

There's also nothing special about subclasses, e.g. we could use tuples:

``` python
Left  = lambda x: (True , x)
Right = lambda y: (False, y)

isLeft  = lambda x: x[0]
isRight = lambda x: not (isLeft(x))

fold = lambda x, f, g: f(x[1]) if isLeft(x) else g(x[1])
```

Yet regardless of how we implement `fold`, it's still about as verbose and
redundant as my original `isinstance` example, or the pattern-matching version.

Hence many programmers will abstract away this repeated behaviour, i.e. the idea
of "transform successful results, leave errors as-is".

The most common is `map`, which transforms values while leaving the "container"
untouched:

``` python
map = lambda x, f: x if isLeft(x) else Right(f(x[1]))
```

Hence `map(Left(x), f)`{python} will return `Left(x)` unchanged, whilst
`map(Right(y), f)`{python} will return `Right(f(y))`.

The next most useful abstraction is `product`, which lets us combine two
"containers" into a single container with a pair of values:

```python
product = lambda x, y: x if isLeft(x) else (
  y if isLeft(y) else Right((x[1], y[1]))
)
```

Hence `product(Right(x), Right(y))`{python} gives `Right((x, y))`{python}, but
if either of the arguments to `product` is a `Left`, we'll get back a `Left`
(if *both* arguments are `Left`, this implementation happens to return the first
one; we could write an equivalent function which returns the second instead, or
we could even combine them with some merging function, specifically if the
values wrapped in the `Left` form a "semigroup", but that's a whole other
discussion!).

There is an alternative to `product` called `app`, where the first value is a
function and the second is its argument:

```python
app = lambda f, x: f if isLeft(f) else (
  x if isLeft(x) else Right(f[1](x[1]))
)
```

`product` and `app` are equivalent, since we could also define `app` like this:

```python
def app(f, x):
  pair = product(f, x)
  return map(pair, lambda p: p[0](p[1]))
```

Or we could define `product` like this:

```python
product = lambda x, y: app(map(x, lambda a: lambda b: (a, b)), y)
```

We can also define versions with more arguments, to produce larger tuples (in
the case of `product`) or call functions with more arguments (in the case of
`app`).
