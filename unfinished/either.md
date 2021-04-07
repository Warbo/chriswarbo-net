> In languages with better ADT/pattern matching support (like Haskell or Rust) you'd use case/match operator, or whatever it's called.

When chaining lots of calls together it's usually a better idea to use some combination of `map`, `app`/`product` and `flatMap`/`join` (these functions are usually provided by interfaces called functor, applicative and monad, respectively).

For example:

    def example(a):
      b = foo(a)
      if isinstance(b, Exception): return b
      c = bar(b)
      if isinstance(c, Exception): return c
      d = baz(quux(c))
      if isinstance(d, Exception): return d
      return foobar(d)

You're right that in Haskell/ML/Scala/etc. we would probably use `Either[Exception, T]` (Scala has a type called `Try[T]` which is essentially the same; its values are `Success(T)` and `Failure(Throwable)`).

We *could* pattern-match on them, but that gets tedious quite quickly, e.g. in Scala

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

We've not really gained anything here, since the `isinstance...: return...` lines have just become `case Left(e) => Left(e)`. Note that we don't need pattern-matching for this either; we can do the same thing using functions (a form of Church encoding) in any of these languages, including Python, like this:

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

This assumes we have:

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
        return f(self.y)

There's nothing particularly special about using dynamic dispatch; we could define a standalone function instead, like:

    fold = lambda x, f, g: f(x.x) if isinstance(x, Left) else g(x.y)

(Note that this is basically like my first example, but the `isinstance` conditional has been turned into a function with the branches at arguments). There's also nothing special about subclasses, e.g.

    Left  = lambda x: (True , x)
    Right = lambda y: (False, y)

    fold = lambda x, f, g: f(x[1]) if x[0] else g(x[1])
    

Yet regardless of how we implement `fold`, it's still about as verbose and redundant as my original `isinstance` example, or the pattern-matching version.

Hence many programmers will abstract away this repeated behaviour, i.e. the idea of "transform successful results, leave errors as-is".

The most common is `map`, which transforms values while leaving the "container" untouched:

    map = lambda x, f: x if x[0] else (False, f(x[1]))

The next most useful is `product`, which lets us combine two "containers" into a single container with a pair of values:

    product = lambda x, y: x if x[0] else (y if y[0] else (False, (x, y)))

This is equivalent to 'app', which treats the values as the function and argument 
