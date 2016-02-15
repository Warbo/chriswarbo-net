---
title: PowerPlay implementations
dependencies: static/summariseTables
postprocessor: ./static/summariseTables
---
I've begun writing some implementations of the PowerPlay spec I've talked about
[elsewhere](/essays/powerplay).

## Lookup Tables ##

The first one can be found in Sqrt.v and is a very simple proof-of-concept. The problem it tries to solve is finding integer square roots, which it does using a lookup table. This may sound odd, but it has a nice result: since the implementation language (lookup tables) can never be perfect (there will always be numbers missing), it's quite straightforward to make a system which keeps improving itself forever. However, since our meta-language (Coq) can solve the square root problem quite easily, we don't run into any difficulties in our proofs. The result is an ever-increasing list of numbers and their square roots, which Coq curries into our interpreter to make solver functions:

<div class="summarise">
<span class="summary">Execution trace of Solver</span>

+-----------+--------------+------------------------------------------------------+
| Iteration | Lookup Table | Auto-generated Solver                                |
+===========+==============+======================================================+
|     0     | ```          | ```ocaml                                             |
|           | {}           | fun p n => None                                      |
|           | ```          | ```                                                  |
+-----------+--------------+------------------------------------------------------+
|     1     | ```          | ```ocaml                                             |
|           | {0 => 0}     | fun p n =>                                           |
|           | ```          |   match 0 == p with                                  |
|           |              |     | left  r => Some (srl_convert r (sqrt 0))       |
|           |              |     | right r => None                                |
|           |              |   end                                                |
|           |              | ```                                                  |
+-----------+--------------+------------------------------------------------------+
|     2     | ```          | ```ocaml                                             |
|           | {1 => 1,     | fun p n =>                                           |
|           |  0 => 0}     |   match 1 == p with                                  |
|           | ```          |     | left  r => Some (srl_convert r (sqrt 1))       |
|           |              |     | right r => match 0 == p with                   |
|           |              |       | left  r => Some (sqrt 0)                     |
|           |              |       | right r => None                              |
|           |              |     end                                              |
|           |              |   end                                                |
|           |              | ```                                                  |
+-----------+--------------+------------------------------------------------------+
|     3     | ```          | ```ocaml                                             |
|           | {9 => 3,     | fun p n =>                                           |
|           |  1 => 1,     |  match 9 == p with                                   |
|           |  0 => 0}     |    | left  r => Some (srl_convert r (sqrt 3))        |
|           | ```          |    | right r => match 1 == p with                    |
|           |              |      | left  r => Some (srl_convert r (sqrt 1))      |
|           |              |      | right r => match 0 == p with                  |
|           |              |        | left  r => Some (srl_convert r (sqrt 0))    |
|           |              |        | right r => None                             |
|           |              |      end                                             |
|           |              |    end                                               |
|           |              |  end                                                 |
|           |              | ```                                                  |
+-----------+--------------+------------------------------------------------------+
|     4     | ```          | ```ocaml                                             |
|           | {49 => 7,    | fun p n =>                                           |
|           |   9 => 3,    |  match 49 == p with                                  |
|           |   1 => 1     |    | left  r => Some (srl_convert r (sqrt 7))        |
|           |   0 => 0     |    | right r => match 9 == p with                    |
|           | ```          |      | left  r => Some (srl_convert r (sqrt 3))      |
|           |              |      | right r => match 1 == p with                  |
|           |              |        | left  r => Some (srl_convert r (sqrt 1))    |
|           |              |        | right r => match 0 == p with                |
|           |              |          | left  r => Some (srl_convert r (sqrt 0))  |
|           |              |          | right r => None                           |
|           |              |        end                                           |
|           |              |      end                                             |
|           |              |    end                                               |
|           |              |  end                                                 |
|           |              | ```                                                  |
+-----------+--------------+------------------------------------------------------+
| ...       | ...          | ...                                                  |
+-----------+--------------+------------------------------------------------------+

</div>

Notice how the code works: our branches are **not** checking for different values of `p`, since that would look like this:

```ocaml
match p with
  | 49 => ...
  | 9  => ...
  | 1  => ...
  | 0  => ...
end
```

Instead, let's trace what happens if we use `p = 9` (`n` is not used; it's only there so we satisfy the spec):

```ocaml
match 49 == p with
  | left  r => Some (srl_convert r (sqrt 7))
  | right r => match 9 == p with
    | left  r => Some (srl_convert r (sqrt 3))
    | right r => match 1 == p with
      | left  r => Some (srl_convert r (sqrt 1))
      | right r => match 0 == p with
        | left  r => Some (srl_convert r (sqrt 0))
        | right r => None
      end
    end
  end
end
```

First we calculate `49 == p`, which gives us a value `right r`, where `r` is a proof that `not(49 = p)`. We pattern-match against this, which reduces to the following:

```ocaml
match 9 == p with
  | left  r => Some (srl_convert r (sqrt 3))
  | right r => match 1 == p with
    | left  r => Some (srl_convert r (sqrt 1))
    | right r => match 0 == p with
      | left  r => Some (srl_convert r (sqrt 0))
      | right r => None
    end
  end
end
```

Next we calculate `9 == p`, which gives us a value `left r`, where `r` is a proof that `9 = p`. We pattern-match against this, which reduces to the following:

```ocaml
Some (srl_convert r (sqrt 3))
```

The `sqrt` function is a constructor for our solutions. The type it constructs is indexed by the square of its argument, so `sqrt 3` constructs a value of type `Sqrt (3 * 3)`. In order to convince Coq that we're solved the problem, we need to coerce `sqrt 3` to a value of type `Sqrt p`, which the `srl_convert` function will do, using the proof `r` that `9 = p`.

The reason we keep adding new tests to the start of the solver, rather than the end, is because we implement the lookup table with a list, which we prepend to.

The reason the numbers progress like this (0, 1, 3, 7, ...) is that in each iteration, we count down from our timeout until we find a number who's square isn't in the table. This guarantees that we'll halt. Since the PowerPlay spec doubles the timeout on each iteration, we get timeouts of 1, 2, 4, 8, ..., so in iteration n we will prepend the square of (2^n - 1) to our lookup table.

With this proof-of-concept under my belt I've moved on to tackling undecidable problems with a universal programming language. I initially considered untyped lambda calculus, but decided that binding contexts were too much hassle. Instead, I've used one of my favourite programming languages: [SK combinator calculus] [3].

[3]: http://en.wikipedia.org/wiki/SKI_combinator_calculus

I've actually implemented a more general hierarchy of calculi, which each have access to a different (finite) number of placeholders. These act as meta-level variables, so for example anything of type `SK 10` can contain the usual S and K combinators (written `cS` and `cK`, to avoid conflicting with the `S` constructor of `nat`), application (written `cA x y`) and 10 distinct variables (`cV F1`, `cV (FS F1)`, `cV (FS (FS F1))`, ... `cV (FS (FS (FS (FS (FS (FS (FS (FS (FS F1)))))))))`). This makes `SK 0` equivalent to the regular SK calculus.

These variables have no computational meaning (they're never instantiated with values), but they're useful for tracing the execution of a combinator. For example, we can take a term `c : SK 0` (ie. a regular, variable-free combinator) and apply it to two distinct arguments: `cA (cA c (cV F1)) (cV (FS F1))` then beta-reduce it some number of times. If the result is exactly `cV F1` or `cV (FS F1)` then we know that `c` is a Church-encoded boolean. We will arbitrarily choose that combinators returning `cV F1` are TRUE and those returning `cV (FS F1)` are FALSE.

Note that we have to use distinct variables like this; we can't just pass in `cS` and `cK` as arguments and check for them in the result, since the result might be beta/eta equivalent to `cS` or `cK` and we wouldn't necessarily know (since working it out is undecidable). Since the variables never beta-reduce, we eliminate beta-equivalent results. We may still get eta-equivalent terms, but that's unavoidable due to functional extensionality being undecidable.

Now that we've got a semi-reasonable way to extract Church-encoded results from combinators, we can use them to encode problems. Specifically, we use Problems of the form `(c : SK 0, n : nat)` and Solutions of the form `a : SK 0` such that `cA c a` reduces to a Church-encoded TRUE in (at most) `n` steps.

In other words, we can express any Problem we want to solve as a function which categorises guesses as those which are Solutions (return value of TRUE) or not (return value of anything except TRUE). We then Church-encode everything and express it as SK combinators.

Note that putting the time limit `n` in the Problem, rather than the Solution, Solvers which produce faster Solutions will Dominate those with slower Solutions (all else being equal).

Now, how do we implement Solvers? Well, any way we like. However, it's quite nice to use `SK 0` as our AST type; that way we can take the machinery we made for running candidate Solutions through Problems and repurpose it for interpreting Solvers. This is quite straightforward.

The next thing I want to do is implement a decent Searcher, eg. using Levin Search, to look for replacement Solvers. I'm not sure how I'll tackle the Improvement criterion yet; maybe by restricting the search to those Solvers which subsume previous ones, only doing their own processing when the previous Solver returns None.

Once I've got that in place then in principle I'll have a universal problem solver. The next step will be to make another implementation, using Coq-in-Coq, which will remove the restrictions on self-improvement (all provable improvements will be allowed).

The final step will be to 'tie the knot' by making Searcher a subtype of Solver (hence making State a subtype of Problem and Improvement a subtype of Solution). This way, the algorithm can improve its own improvement-finding algorithm.
