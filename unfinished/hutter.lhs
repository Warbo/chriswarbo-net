Hutter Search takes a program f and an input x and finds the result of f(x) in a provably-optimal way. The algorithm runs a brute-force enumeration of proofs in some formal system. When it finds a proof that f(x) = f'(x) for some program f', and that f' is faster than f, it starts running f' instead.

Let's make this more concrete. We'll use the untyped Lambda Calculus to construct our programs and use Type Theory as our formal system. An Idris implementation would look something like this:

[code="idris"]
Helper functions

> -- Decrement a Peano-encoded Natural number
> dec :: Nat -> Nat
> dec  Z    = Z  -- Decrementing zero gives zero
> dec (S n) = n  -- Decrementing the successor of n gives n

> -- Maximum of two Peano-encoded Natural numbers
> max   Z     n    = n
> max   n     Z    = n
> max  (S n) (S m) = S (max n m)

Data types for Lambda Calculus programs

> -- Raw Lambda Calculus expressions
> data Expr : Type where
>   Lambda : Expr    -> Expr          -- Function definition
>   App    : Expr    -> Expr -> Expr  -- Function application
>   Var    : (Fin n) -> Expr          -- Variable with de Bruijn index
> instance

> -- Scoped wraps an Expr and keeps track of its free variables.
> -- Nat is how many Lambdas we need to close Expr.
> data Scoped : Nat -> Expr -> Type where
>   -- Functions decrement the closure counter, due to their argument
>   Lambda :    (Scoped      n           e )
>            -> (Scoped (dec n)  (Lambda e))
>
>   -- Function application doesn't care about closure
>   App    :    (Scoped  n         f       )
>            -> (Scoped  m         a       )
>            -> (Scoped (max n m) (App f a))
>
>   -- Variables have a constant closure counter
>   Var    : (n : Fin m) -> Scoped m (Var n)

> scope : (e : Expr) -> Scoped n e
> scope (Lambda e) = Lambda (scope e)
> scope (App f x)  = App (scope f) (scope x)
> scope (Var n)    = Var n

How to evaluate a Lambda Calculus expression

> -- One step of β reduction
> beta :    (Scoped n    e)
>        -> (Vect   Expr n)
>        ->  Expr
> beta (Var n)            es = index n es
> beta (Lambda e)         es = Lambda e
> beta (App (Lambda x) e) es = beta x (e : es)
> beta (App x e)          es = App (beta x) (beta e)

> normal ::

> -- Partial lets us express diverging computations as co-data values
> data Partial a = Now a
>                | Later (Partial a)
> instance Monad Partial where
>   return x        = Now      x
>   (Now   x) >>= f = Now   (f x)
>   (Later x) >>= f = Later (f >>= x)

> -- Full β reduction
> reduce : (Scoped 0 e) -> (Vect Expr 0) -> Partial Expr
> reduce e = betaReduce e []
