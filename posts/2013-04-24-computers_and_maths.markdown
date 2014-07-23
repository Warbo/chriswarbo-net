---
title: Computers and Maths
---
I just read [No, computers can't be trusted with math's future](http://worldofweirdthings.com/2013/03/12/no-computers-cant-be-trusted-with-maths-future/) and thought I'd write some clarifying remarks, since I've been playing around with proof assistants for a while now.

It is in response to a Wired article [As Math Grows More Complex, Will Computers Reign?](http://www.wired.com/wiredscience/2013/03/computers-and-math/all/), which mentions there are several ways that computers are used in Maths. However, I think it's missed a biggie: the [Curry-Howard correspondence](http://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence).

The Curry-Howard Correspondence tells us that [datatypes](http://en.wikipedia.org/wiki/Data_type) and [logical formulas](http://en.wikipedia.org/wiki/Mathematical_logic) are the same thing, and that a computer programs of a given type are the same as proofs of a given formula.

In the context of computers and Maths, programming with paper + pen isn't much good, but when done on a computer then a whole world of possibilities opens up (not least of which is the ability to run things!). There are [many systems](http://en.wikipedia.org/wiki/Category:Dependently_typed_languages) which demonstrate that programming and theorem proving are the same thing.

The Curry-Howard correspondence shows Prof Teleman's arguments against computing in the Wired article to be quite ironic; he "wished he knew how to program so he could calculate the solution to a problem", when actually he's been programming every time he's written a proof! It reminds me of [something I saw on a blog recently](http://www.free-variable.org/2013/04/mind-the-gap-please/):

> An eminent Comp Sci professor was asked what was the best programming language. Prof. paused, then answered "graduate student".

To show Curry-Howard in action let's apply it to [Fermat's Last Theorem](http://en.wikipedia.org/wiki/Fermat's_Last_Theorem), which is a logical statement "Given any Natural numbers n, a, b and c, the statement a^(n+3) + b^(n+3) = c^(n+3) is False". Curry-Howard tells us this is a datatype, so let's write it down in a programming language (in this case [Idris](http://idris-lang.org/)):

```haskell
fermatsLastTheorem : Type
fermatsLastTheorem = (Equal (a^(n+3) + b^(n+3)) (c^(n+3))) -> False

fermatsLastTheoremProof : fermatsLastTheorem
fermatsLastTheoremProof eqProof = ...
```

In Idris `X -> Y`{.haskell} is the type of functions which have argument type `X`{.haskell} and return type `Y`{.haskell}. If we define `+`{.haskell} and `^`{.haskell} to work on Natural numbers then Idris will infer that `n`{.haskell}, `a`{.haskell}, `b`{.haskell} and `c`{.haskell} are Natural numbers. So what are `Equal`{.haskell} and `False`{.haskell}?

`Equal`{.haskell} turns two values into a type, but only types like `Equal x x`{.haskell} have a value. Hence we can only have a value of `Equal x y`{.haskell} if `x`{.haskell} and `y`{.haskell} are the same thing:

```haskell
data Equal : a -> a -> Type where
  eqSelf : (x : t) -> Equal x x  -- Ignore t, duplicate x
```

`False`{.haskell} is a type with no values, ie. a statement with no proof. Idris calls this `_|_`{.haskell}, so we can re-use that:

```haskell
type False = _|_
```

Now our code won't compile until we replace `...`. What can we put in its place? The compiler wants a value of type `False`{.haskell}, but we just said that `False`{.haskell} has no values! The way around this is the [principle of explosion](http://en.wikipedia.org/wiki/Principle_of_explosion) or "from a contradiction, anything follows". If we can show that our argument `eqProof`{.haskell} leads to a contradiction, we can prove anything and hence we can prove `False`{.haskell}. Now one of two things could be the case:

 1) We can build a return value and compile our program.
 1) We can't build a return value and the compiler keeps rejecting our program.

If (1) is the case then `eqProof`{.haskell} must lead to a contradiction, hence `Equal (a^(n+3) + b^(n+3)) (c^(n+3))`{.haskell} has no values, hence `a^(n+3) + b^(n+3)`{.haskell} is not the same as `c^(n+3)`{.haskell} for any values of `n`{.haskell}, `a`{.haskell}, `b`{.haskell} or `c`{.haskell}. In other words, if we get our program to compile, we've proved Fermat's Last Theorem!

If (2) is the case then either Fermat's Last Theorem is wrong or [our language can't express the proof](http://en.wikipedia.org/wiki/G%C3%B6del's_incompleteness_theorems), but [we can't tell which](http://en.wikipedia.org/wiki/Constructivism_(mathematics)).

An interesting result is that our program has done its job once it's compiled; it never has to run. In fact, the resulting executable won't actually do anything anyway: `fermatsLastTheorem`{.haskell} is a type, so it doesn't do anything at run-time; `fermatsLastTheoremProof`{.haskell} is a function, but in order to compile it we must prove that it can never be given an argument!

What's going on is that we've expressed our Mathematical statements using [Type Theory](http://en.wikipedia.org/wiki/Type_theory), which lets us use our compiler's type-checker as a proof-checker.

Of course, this forces us to trust the compiler, but this is OK as long as the language is [total](http://en.wikipedia.org/wiki/Total_functional_programming). Since we don't care about running our programs, there's no need to do anything fancy like optimisation, or even code generation, so the compiler can be small, simple and stupid (no clever tricks), and hence [more trustworthy](http://en.wikipedia.org/wiki/Automated_proof_checking). We never need to trust our own programs.

### How does this impact what's in the article? ###

Firstly, if programs are proofs then long programs are inevitable because [long proofs are inevitable](http://en.wikipedia.org/wiki/G%C3%B6del's_speed-up_theorem). As programmers we're used to making tradeoffs between program complexity and efficiency; optimising code usually makes it longer, simplifying code usually makes it slower. However, Curry-Howard doesn't care what a program *does*, it only cares what a program *is*, which makes these tradeoffs more difficult.

In the example above, the code doesn't do anything when executed, so the codebase will consist entirely of type coercions, which isn't easy to optimise in a way that the compiler will accept. Our job is not to produce a particular value by whatever means we can, it is to convince the small, simple, stupid compiler that the value we already have is correct!

Now, there are ways to simplify these programs by moving to a higher level, most notably using [proof tactics](http://coq.inria.fr/V8.1/refman/Reference-Manual011.html): heuristic algorithms which try to construct proofs. However, these days our most sophisticated tactics still amount to brute-force search and even then aren't considered generic enough to re-use in different projects.

Now, both articles are correct about the computers' lack of ability to abstract, create analogies, etc. but one area addressing this is [artificial curiosity](http://www.idsia.ch/~juergen/interest.html), but it's very early days.

Personally I've been playing around trying to make an artificially curious proof tactic, but unfortunately those tactic languages I've tried (Coq's Ltac and OCaml plugins, Idris's reflection system) are pretty anthropocentric; for example, they use strings for names rather than de Bruijn indices and there's no generic way to deconstruct types (eg. into sums, products, functions, etc.). Working around this with a custom language is also difficult, since embedding one dependently typed language into another is still very cutting-edge. Still, once I've figured it out it shouldn't take long to hook it up to PowerPlay and watch it explore (very slowly) :)
