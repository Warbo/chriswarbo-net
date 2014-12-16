---
title: More Lambda Lounge
---

As mentioned in an earlier post, I went to a presentation on monads in Scala at Manchester Lambda Lounge and played around with Scala afterwards.

Let's compare three implementations of the Maybe monad; one in Scheme, one in Haskell and what we ended up with in Scala (modulo some mis-rememberings and syntax errors by me, since I'd never read any Scala before last night!). In all three cases we write a function `foo` which passes `10` into the monad and uses `bind` to convert it into a string `"10"`:

```scheme
;; We represent monads using a pair of '(return bind) implementations,
;; and we pull them out using "return" and "bind"
(define (monad r b) '(r b))
(define return car)
(define bind  cadr)

;; We represent Maybe using empty or singleton lists
(define nothing  '())
(define (just x) '(x))

;; Maybe's return and bind functions wrap and unwrap these lists
(define maybeReturn just)
(define (maybeBind x f) (if (eq? x nothing)
                            nothing
                            (f (car x))))
(define maybe (monad maybeReturn maybeBind))

;; Our "foo" function takes a monad as an argument
(define (foo m)
        ((bind m) ((return m) 10)
                  (lambda (x) ((return m) (number->string x)))))

;; Execute, by passing in the Maybe monad
(foo maybe)
```

```haskell
-- We define some of our monad interface using a typeclass
class Monad m where
  return :: a -> m a
  >>=    :: m a -> (a -> m b) -> m b

-- We represent Maybe with its own data type
data Maybe a = Nothing
             | Just a

-- We specify Maybe's bind and return in its typeclass instance
instance Monad Maybe where
  return         = Just
  Nothing  >>= _ = Nothing
  (Just x) >>= f = Just (f x)

-- foo doesn't need a formal argument
foo :: (Monad m) => m String
foo = return 10 >>= (return . show)
```

```scala
// Monad is a trait
trait Monad[A, M] { self : M =>
  def _return(x: A): M[A]
  def >>=[B](x: M[A], f: A -> M[B]) : M[B]
}

// Maybe implements Monad using a private variable
abstract class Maybe[A] extends Monad[A, Maybe[A]]

class Nothing extends Maybe[A] {
  override def _return(x: A) = null  // Useless
  override def >>=[B](x: Maybe[A])
}

class Just(x : A) extends Maybe[A] {
  val y = x
}
```

The problem is that the Scala version doesn't know *which* monad it's referring to. The ad-hoc polymorphism exposed by Haskell's typeclasses uses "dictionary-passing"; essentially equivalent to the Scheme passing around the monad implementation explicitly, but done implicitly by the language. We would like Scala to do the same, but