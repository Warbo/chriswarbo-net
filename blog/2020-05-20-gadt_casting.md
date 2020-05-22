---
title: GADT Casting
---

## The Setup ##

I recently hit a problem in Haskell, where I had a GADT of the following form:

```haskell
data Command m a where
  Foo ::                  Command m Bool
  Bar ::        String -> Command m Bool
  Baz :: Int -> String -> Command m Bool
```

(In fact I'm using `newtype` to avoid "stringly typed programming", but that's
not relevant to this post)

This GADT takes two type parameters:

 - `m` is unconstrained. We can unify it with anything, and all of the
   constructors will work.
 - `a` is always `Bool`. We can *state* the type `Command m a` for any choice of
   `a` we like; but there are no *values* of that type unless `a` happens to be
   `Bool`.

I ended up with this while implementing an effect using the excellent `polysemy`
library. In `polysemy` we can define "effects" at the type level as entities
with two parameters (like `Command`). Their constructors define the "actions"
that result in such effects (like `Foo`, `Bar x` and `Baz x y`). The `m`
parameter appears to be boilerplate (I assume it's some underlying `Monad`?)
whilst the `a` parameter is the type of an action's return value (we can use
`()` for an uninformative "null" value, or `Void` to indicate that it never
returns anything (e.g. it crashes or loops forever); see
[my post on those types](/blog/2020-02-09-bottom.html)). In this example
`Foo`, `Bar x` and `Baz x y` all use `Command` effects to produce a `Bool`.

Elsewhere we need to define at least one *interpreter* for this effect, which
will turn actions using `Command` into some other type of action: usually
something more general (but more dangerous) like `IO`; or some pure, in-memory
calculation. We often have two interpreters: one "real" interpreter for writing
programs, and one in-memory interpreter for testing.

In my case I want to implement these `Command` actions using `IO`, by invoking
certain shell commands. I don't care about the stdio of those commands, but I
*do* care whether or not they exit successfully: that's why they all yield a
`Bool`.

My interpreter looks something like this:

```haskell
commandToIO1 :: Member (Embed IO) r => Sem (Command ': r) a -> Sem r a
commandToIO1 = interpret (embed . command)
  where command :: Command m a -> IO a
        command c = case c of
          Foo     -> run ["foo"           ]
          Bar x   -> run ["bar",      x   ]
          Baz x y -> run ["baz", show x, y]

        run :: [String] -> IO Bool
        run args = do
          let cmd = proc "myCommand" args
          (code, _, _) <- readCreateProcessWithExitCode cmd ""
          pure $ case code of
            ExitFailure _ -> False
            ExitSuccess   -> True
```

The type of `commandToIO1` tells us that it turns actions with access to the
`Command` and `IO` effects (among others) into actions which no longer involve
`Command` (the `Embed` and `embed` boilerplate is due to `IO` not taking an `m`
parameter). We achieve this by using the `command` function to replace any
`Command m a` action with an equivalent `IO a` action; in this case invoking a
shell command via `run`.

## The Problem ##

The above interpreter works fine, but I wanted to write some tests, to check
that we're generating plausible arguments for each action. That seems easy
enough, we just need to pull the argument selection out of the interpreter:

```haskell
commandArgs :: Command m a -> [String]
commandArgs c = case c of
  Foo     -> ["foo"           ]
  Bar x   -> ["bar",      x   ]
  Baz x y -> ["baz", show x, y]

commandToIO2 :: Member (Embed IO) r => Sem (Command ': r) a -> Sem r a
commandToIO2 = interpret (embed . command)
  where command :: Command m a -> IO a
        command c = run (commandArgs c)

        run :: [String] -> IO Bool
        run args = do
          let cmd = proc "myCommand" args
          (code, _, _) <- readCreateProcessWithExitCode cmd ""
          pure $ case code of
            ExitFailure _ -> False
            ExitSuccess   -> True
```

Yet `commandToIO2` fails to type check! Why?

The problem is that the return value of `command` should have type `IO a`, where
`a` comes from the type of its argument `Command m a`; but we're giving back the
return value of `run`, which is an `IO Bool`. GHC correctly infers that `a`
needs to equal `Bool`, but it can't prove that it does, so it gives up.

So why did the previous version work, when that *also* gave back the return
value of `run`? The difference is *context*: each time `commandToIO1` returns
`run [...]`, it's in a specific branch of a `case` statement: one for each
constructor. Since the constructors are all specialised to `Bool`, matching one
of those patterns gives us some extra information: that `a` is equal to `Bool`,
and hence it's OK to return an `IO Bool`.

## Failed Solutions ##

### Failure 1 ###

How can we avoid this problem, whilst still selecting the arguments with a
separate, easily testable function? Maybe we should move the call to `run` back
into the `case` branches? We can keep `run` encapsulated inside `commandToIO3`
by passing it in as a parameter:

```haskell
commandArgs2 :: ([String] -> t a) -> Command m a -> t a
commandArgs2 f c = case c of
  Foo     -> f ["foo"           ]
  Bar x   -> f ["bar",      x   ]
  Baz x y -> f ["baz", show x, y]

commandToIO3 :: Member (Embed IO) r => Sem (Command ': r) a -> Sem r a
commandToIO3 = interpret (embed . command)
  where command :: Command m a -> IO a
        command c = commandArgs2 run c

        run :: [String] -> IO Bool
        run args = do
          let cmd = proc "myCommand" args
          (code, _, _) <- readCreateProcessWithExitCode cmd ""
          pure $ case code of
            ExitFailure _ -> False
            ExitSuccess   -> True
```

Unfortunately this is not enough: `commandToArgs2` is happy to unify `a` with
`Bool`, due to its `run` argument, since (once again) we're only calling that
from the `case` branches where the context contains proof that it's valid.

However, `command` is certainly *not* happy that (yet again) it's being given an
`IO Bool` when it wants an `IO a`: the evidence that `a` equals `Bool` was
discovered during the type checking of `commandArgs2 run c` but thrown away once
the type checker left those `case` branches.

### Failure 2 ###

Incidentally, you might wonder why we don't just change the type of `command` to
return an `IO Bool`: if we try that, we get the same problem about not being
able to unify `a` with `Bool`; except this time from `polysemy`'s `interpret`
rather than our `command` function.

### Failure 3 ###

Perhaps we could generalise the return type of `run`, so it gives an `IO a`
rather than an `IO Bool`? That's a non-starter, since we're explicitly returning
`True` or `False` on the last two lines (remember, returning a value of type
`a` doesn't need to work for *some* type `a`; it must work *for all* types `a`
(subject to any constraints, but we don't have any here)).

### Failure 4 ###

One way to avoid type errors is to re-introduce the pattern-matching that gave
`command` the context it needed to spot that `a` is always `Bool`. It's easy
enough to do this:

```haskell
commandArgs :: Command m a -> [String]
commandArgs c = case c of
  Foo     -> ["foo"           ]
  Bar x   -> ["bar",      x   ]
  Baz x y -> ["baz", show x, y]

commandToIO4 :: Member (Embed IO) r => Sem (Command ': r) a -> Sem r a
commandToIO4 = interpret (embed . command)
  where command :: Command m a -> IO a
        command c = let result = run (commandArgs c)
                     in case c of
                          Foo     -> result
                          Bar _   -> result
                          Baz _ _ -> result

        run :: [String] -> IO Bool
        run args = do
          let cmd = proc "myCommand" args
          (code, _, _) <- readCreateProcessWithExitCode cmd ""
          pure $ case code of
            ExitFailure _ -> False
            ExitSuccess   -> True
```

This works, but is pretty horrible: we've introduced a whole load of boilerplate
into our interpreter, for no reason other than appeasing the type checker. This
boilerplate also has to be kept up to date when changes are made to the
`Command` effect (e.g. changing the constructors or their arguments). Another
reason I count such boilerplate as a failure is that it isn't constant: every
time we want to solve this problem, we have to do it all again (it's also worth
noting that my actual GADT currently has nine constructors, rather than three!).

## A Solution: Casting ##

The solution I came up with still involves pattern-matching boilerplate, but we
abstract it out into a reusable function (as any good programmer should!). We
can think of our function as providing the evidence that the `a` parameter is
equal to `Bool`, and it takes the form of a *cast*:

```haskell
castCommand :: Command m a -> Bool -> a
castCommand c b = case c of
  Foo     -> b
  Bar _   -> b
  Baz _ _ -> b
```

This type-checks, for the same reason we could use `run` in each `case` branch
of `command`: the pattern-matching gives us the context to show that `a` is
`Bool` in every case. We can call this function whenever we need a value of
type `a` but only have a `Bool`:

```haskell
commandToIO :: Member (Embed IO) r => Sem (Command ': r) a -> Sem r a
commandToIO = interpret (embed . command)
  where command :: Command m a -> IO a
        command c = castCommand c <$> run (commandArgs c)

        run :: [String] -> IO Bool
        run args = do
          let cmd = proc "myCommand" args
          (code, _, _) <- readCreateProcessWithExitCode cmd ""
          pure $ case code of
            ExitFailure _ -> False
            ExitSuccess   -> True
```
