---
title: State, Effects and Monads in Functional Programming and Haskell
---
This is a long reply to [a comment on Reddit](
https://www.reddit.com/r/programming/comments/6mqxh2/pragmatic_functional_programming/dk5kxye)
which became too long to submit there ;)

User [gnus-migrate](https://www.reddit.com/user/gnus-migrate) says:

> Changing a system's state does not mean overwriting a variable's reference.
> It's a more general idea than that.

Of course, I agree. They go on:

> For example if you type a word in a text field, you're changing the state of
> your browser. If you store a field in a web app's database you're changing the
> state of that web app. If you interface with the outside world in any shape or
> form you are dealing with state, and no amount of immutability will protect
> you from that.

Yet these things *can't be expressed* in the semantics of FP (beta-reduction of
lambda abstractions). Sure, you can bolt on some extra things to a language's
semantics, like the imperative execution of side-effects found in Scheme and ML,
or the execution of the `main` IO action by Haskell's runtime system, but that's
just moving the state *out* of the FP part and implementing it with something
else. That's a perfectly reasonable thing to do, but by managing to *avoid* the
problem (of state in FP), such approaches don't *solve* it (since they don't
need to). Hence we can't point at them as "how state works in FP", since the
whole idea is that the bit we're pointing to isn't the bit that's FP.

My example [in the parent comment](
https://www.reddit.com/r/programming/comments/6mqxh2/pragmatic_functional_programming/dk446b0)
showed how to *emulate the behaviour of* a particular form of state (mutable
variables), using only immutable data and recursion (which *can* be expressed in
FP semantics; e.g. with Church encoding). We can do the same thing for these
examples, by building up an immutable sequence tracing out the *entire history*
of the resource (like a [block universe](
https://en.wikipedia.org/wiki/Eternalism_(philosophy_of_time))); whether that's
a variable (like I showed), a browser (which I've never seen done) or a database
(AKA "log-structured storage").

> It's obvious that you still have to manage state at the macro level right?

I don't think it is. Sometimes we can just fill up disk/RAM by evaluating more
and more of an infinite immutable data structure, since we know that we'll be
finished with it long before it gets too big. This is stateless (but
incrementally evaluated). In this sense, garbage collection is an optimisation,
and we can sometimes get away without it (relying on the OS to reclaim all
memory when we're finished).

Often such naive "block universe" implementation is inadequate for our resource
constraints, so we're required to find some optimisation (like garbage
collection, or tail-call elimination, or collapsing together log entries,
etc.). Optimisations don't alter the semantics, so our programming model and
mental model can remain the same, but they allow the implementation to run
within the available resources. Such optimisations may make use of state, but
*only* in a way which doesn't alter the semantics, and hence in a way that's
"invisible" to FP (in the same way that, say, registers use state in a way
that's invisible to OO objects, L1 cache is stateful in a way that's invisible
to C, etc.)

> Well I need to read a file, how do I do it? Well you need the IO monad, it's
> not really pure so it's sort of an exceptional case in the language

Sorry if you found Haskell difficult (so did I; I gave up 4 times until on my
5th attempt it clicked!). Rather than "not managing state", I actually think
*what you've said here* are the sorts of misconceptions that confuse newcomers
and make things unnecessarily difficult to learn! :)

Let's unpick them. Firstly, you don't "need the IO monad" in Haskell:

 - You never need a monad in any language. They can sometimes be useful though.
 - Monads are nothing more (or less) than a general interface for combining
   particular sorts of values (e.g. lists) in particular ways
   (e.g. concatenation).
 - Monads are *only* useful if you need to combine such values in such ways.
 - Reading a file isn't one of those situations.
 - There are many alternatives to monads, e.g. Haskell has `Functor`,
   `Applicative`, `Arrow`, various forms of algebraic effects, etc.
 - You can always avoid these interfaces and just use the underlying
   datastructures and functions. For example, instead of using
   `join :: (Monad m) => m (m a) -> m a` to flatten a list of lists, you can use
   `concat :: [[a]] -> [a]`; the only thing you lose is polymorphism.
 - If you know you want to use `IO`, since that's the type of `main`, you don't
   need the polymorphism that `Monad` offers.

Hence by trying to use "the IO monad" you're constraining yourself for no
reason: if you want to use `IO`, why limit yourself to its monad interface? If
you want to limit yourself to the monad interface, why hard-code `IO` when the
whole point of `Monad` is to abstract over the type? It's like saying "I need to
dependency inject a `MySqlDatabaseConnection`", when the whole point of
dependency injection is to avoid hard-coding details like the choice of DB:
either inject a `DatabaseConnection`, or just hard-code the
`MySqlDatabaseConnection` and do what you like with it.

Secondly, I take issue with the phrase "the IO monad". If we look at the
definition of
e.g. [readFile](http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html#v:readFile),
where's the "monad"? I see an `IO`, but I see no `Monad`. That's because values
of type `IO a` are just values! *So* many Haskell newcomers get derailed trying
to figure out what it is about `IO` that makes it special and "monadic". The
answer? It's only special because it's in the type of `main :: IO a`; the only
thing which makes it "monadic" is
that
[it has a `Monad` instance](http://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Base.html#line-1093),
but so do lists, `Maybe`, `STM`, `ST`, `Either a`, etc. so that's not
particularly special. `IO` has a `Functor` instance, an `Alternative` instance,
a `Monoid` instance, etc. but we don't get obsessed with understanding the
"alternativeness" of "the IO alternative".

The thing is, the key to how Haskell works *isn't* that `IO` is some magical
type; it isn't (it's
just
[a wrapper around a `State`](https://github.com/ghc/ghc/blob/master/libraries/base/GHC/Base.hs#L1212) which
[contains `Void`](https://github.com/ghc/packages-base/blob/master/GHC/Base.lhs#L751)). Rather,
the "magical" bit is what happens when you call something `main`: inexplicably,
out of nowhere, GHC will *run that value as a program*, simply because we gave
it a particular name! WTF? That's unlike anything else in the entire language,
and (unlike `IO`) there's no way to emulate it in any other way. Yet, nobody
seems to be the least bit phased by this! Except a few who are used to scripting
languages which execute files top to bottom, but even Python has a common idiom
of `if __name__ == "__main__":`, so that confusion doesn't usually last long.

Thirdly, "it's not really pure" is completely wrong. From the links above, we
see that `IO` is just an alternative name for `State RealWorld`. The idea of
`RealWorld` is that it should represent "the state of the world". For example,
let's say we wrote the following program:

    import Control.Monad
    import System.IO
    main = join (fmap putStrLn (readFile "foo.txt"))

GHC will perform various rewrites, optimisations, etc. but it cannot alter the
order in which the `RealWorld` values (hidden in the `IO a` values) are modified
and combined; simply because altering the order in which *any* values are
modified and combined would be incorrect (e.g. consider `(1 + 2) * 3` vs. `1 +
(2 * 3)`), unless we have some sort of equivalence proof.

So we need some way to represent "the state of the world" as a value; as I've
said above, we can emulate state by storing the entire history of everything
that's happened, which we can do with a list. In this case, the `IO String`
returned from `readFile "foo.txt"` will be a pair containing a `String` and a
`RealWorld` something like `["The file 'foo.txt' was read"]`. Likewise, the
`RealWorld` inside the `putStrLn ...` call will be something like `["The string
... was printed"]`.

The `join` function turns an `IO (IO a)` (containing two `RealWorld` values and
an `a`, something like `(world1, (world2, foo))`) into an `IO a` (containing one
`RealWorld` and an `a`). It combines the two incoming `RealWorld` values by
appending the "history" of the outer one to the end of the "history" of the
inner one. Hence the result will be `["The file 'foo.txt' was read", "The string
... was printed"]`. This same process happens for everything involving `IO`:
every "action" that produces an `IO a` is simply writing down a description of
that action in the `RealWorld` it's returning; it's not actually "doing" the
action (since there's no way to even express such a thing in Haskell). Every
function which combines `IO` values, e.g. those in the `IO` instance of `Monad`,
`Applicative`, etc. is just appending these "histories" together (or discarding
them, but such functions usually aren't as useful).

So now we know what a `main` value looks like: it's a pair containing some value
(often `()`), which gets ignored, and a `RealWorld` containing a complete,
ordered history of every "action" that we would *like* the program to take (but
it can't, since there's no such concept in FP). This is all pure, immutable,
etc.

Now here's the key insight: a "list of actions to take" is nothing other than
*an imperative computer program*! Hence the value of `main` (which is the
*return value* of our Haskell code; *not* our Haskell code itself!) actually
defines an imperative program which would, if it were ever executed, *cause* the
world to follow the "history" it describes.

That's the pure FP semantics, but the naive implementation described above would
be very inefficient. We can easily optimise it without altering the semantics:
rather than generating a "history" of the world, and executing it as a program,
we can interleave the two. First we write an interpreter for this separate,
imperative language, then we couple together the evaluation of Haskell code (to
find the next "action" in the "history" in `main`) with the execution of these
actions as statements in the imperative language. Results of the execution are
sent back to be bound as Haskell variables (which, as far as Haskell is
concerned, is the values they've always had) which we can use to evaluate as far
as the following action. The old "actions" can be garbage collected, and the
whole thing is very similar to how tail-call elimination optimises our emulation
of stateful behaviour. In fact, since "actions" are executed as they're
encountered, there's no need to store them anywhere at all; which is why
`RealWorld` is just an empty type.

In this sense, Haskell is basically an elaborate macro system, for a really
rubbish imperative language (one lacking loops, branches, arithmetic,
etc.). Since side-effects (including state) are easily implemented in an
imperative language's semantics, its *that* language's execution that is
impure. Haskell itself just incrementally generates a big list of instructions;
it's up to us whether we want to execute them or not (GHC and GHCi often
conflate evaluation of Haskell with execution of imperative code; Idris doesn't
make this mistake, e.g. see
the
[`:x` instruction in the Idris REPL](http://docs.idris-lang.org/en/latest/reference/repl.html)).

Finally, implementations like GHC go one step further: compiling the Haskell
code (via various intermediate languages) into equivalent imperative code, which
allows one language implementation (usually compiled machine code, but there's
also ghc-js, etc.) to perform both the Haskell evaluation and the imperative
execution.
