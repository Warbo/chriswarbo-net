---
title: Search and Optimisation Streams
---
After an initial spurt of Javascript hacking, the search algorithm pages in my Wiki seem to have stagnated. Have I given up on yet another project? Not quite; I've been building it into a library!

I realised that it's all well and good explaining what search, optimisation, metaheuristics and the like are, but this information is already available elsewhere. The problem with these techniques is not obscurity (most programmers have probably heard of genetic algorithms, at least), and it's not a lack of utility. To me, two of the main problems are:
  1) There are few simple, off-the-shelf implementations available (I don't count convoluted code like `new GALib.GAFactory(crossoverApplicator = new GenericBinaryCrossoverApplicator(0.3777392), mutationRateDeterminator = new GenericAbstractMutatorFactoryBean(2))` as simple!). Those who like to tweak should be able to, but those willing to sacrifice performance for simplicity should be able to.
  2) Lack of API consistency. What if a genetic algorithm turns out to be ill-suited to a problem? Currently, such integration work would probably have to be scrapped, and a different tactic integrated from scratch. If we've already got our infrastructure set up (solution decomposition, fitness functions, etc.), why can't we swap a Genetic Algorithm for a Harmony Search, or a Particle Swarm Optimisation, or a Neural Network, or whatever?

Whilst there are undoubtedly many other problems with current metaheuristics, my library attempts to solve these 2 issues. I've mainly focused on consistency, since I think a few deep, general rules are simpler than many shallow, specific ones. Below, I'll explain the decisions I've made in the process.

The most obvious part of the search pages on my wiki (at least as they existed when I designed the library) is that very little code differs between each technique. Pretty much everything is taken up by the support network (visualisation, resource management, etc.), and the heart of every algorithm can usually be expressed in one line. Added to this, there is a clear relationship between many search algorithms. For example:


**Random choice:** By definition, doesn't follow any pattern. Uses a probability distribution that isn't neccessarily uniform (eg. a decreasing distribution over the Naturals).

**Enumeration:** Follows a deterministic pattern. Requires that all of the possibilities are ordered one-dimensionally. Each solution depends only on the last (ie. it's Markov).

**Random walk:** Requires that all of the possibilities are ordered, but potentially in many dimensions. These orderings are reminiscent of enumerations, as is its Markov property. The random choice taken at each step is, of course, reminiscent of random choice. Note that even with an effective solution topology, it's usually possible to use several coordinate systems (eg. Manhattan, polar, etc.).

**Hill-climbing:** Hill-climbing says that we should back-track a step if a new solution isn't as good as the previous one. This isn't restricted to any algorithm in particular, it can be applied to many.

**Simulated annealing:** This is like hill-climbing, but probabilistically allows solutions with a worse fitness, based on the difference between the fitnesses (big jumps down are less likely) and an ever-decreasing "temperature" (lower temperature means less chance of jumping down). The "temperature" makes this technique very interesting, since the choice of an appropriate temperature gradient is itself a search problem. Note that if we keep the temperature zero, we recover hill-climbing behaviour.

**Tabu search:** Tabu search is similar to hill-climbing, and simulated annealing if we use probabalistic tabus. Tabu search applies a (potentially stateful) predicate to each new solution, and backtracks if the predicate fails. Clearly we can use tabu search to implement a hill-climber, as well as a simulated annealing if we keep internal state for the temperature.

**Mutation:** Mutation treats solutions as being made of separate parts, and swaps some of these at a time rather than the whole solution. Again, this is clearly a general technique which can be used in multiple algorithms, as long as the solutions are amenable to being subdivided.

**Population:** A population keeps track of many solutions at once, so there is more than one "current" solution. Like the above techniques, this is generic enough to apply on top of many other algorithms. The population size, and how to select which solutions to use next, require search algorithms themselves.

**Crossover:** Crossover, like mutation, requires solutions that are subdivided. It also requires a population. It selects a few candidates, then creates new solutions by swapping around some sections of the candidates. Note that we may also treat the selection of crossover points as a search task.

**Artificial selection:** This maintains a population, but requires that the least-fit solutions are discarded if new solutions have higher fitness. This is a form of distributed hill-climbing. We could imagine generalising this analogously to simulated annealing and tabu search. Again, it can be applied on top of many other algorithms.

**Genetic algorithm:** A genetic algorithm maintains a population and applies both crossover and mutation. Sophisticated genetic algorithms are already employing inner search algorithms, eg. to bias the selection and mutation probabilities.

**Extremal optimisation:** This requires that a solution can be broken into parts, and that the fitness of the parts can be determined. The canonical approach is to consider a single solution and to keep replacing the least-fit part with a randomly chosen alternative. However, this can be considered as an amalgamation of a few different strategies, which could be varied: how large the population should be (in the canonical example, a population of 1), how the fitness of a solution's parts determines what is changed (ie. always choosing the least fit part), and how to construct the new solution (random choice, in the canonical version).

**Stream Combinators**

As a wet-behind-the-ears functional programmer, the consistency of this domain told me one thing: use a combinator library. The idea of a library should be familiar to any programmer; a collection of generally useful, hopefully compatible, code which solves common problems. Unfortunately the idea of combinators may not. Simply put, a combinator is a function which accepts some objects, and produces different objects of the same type. For example, addition, subtraction and multiplication are combinators; they accepts numbers and give out numbers. Combinators can have no internal state and cannot cause side-effects (which is why they're popular with functional programmers). With these properties, we are free to combine combinators in arbitrary ways (hence the name); if we have some numbers, we can always add some, subtract some and multiply some (although we can't always divide, due to zero).

In my case, the combinators accept search algorithms and give out search algorithms. Given some search algorithms, we can always send them through our combinators. The obvious thing to do once we've decided on using combinators is to look at a desirable search algorithm, for example a genetic algorithm, and break it down into simpler parts. As stated above, a genetic algorithm needs a population, artificial selection, mutation (which is a form of random walk, which is a form of accumulator) and crossover. In each case, we tease apart the components until we can't get any simpler. While we're at it, we also parameterise the algorithms as much as possible. For example, if we want to bias the mutation of a genetic algorithm, all we should have to do is choose a different random number generator; the rest of the existing functions should carry on working as before (although I'm still working out the best way to implement GAs ;) ).

To represent a search algorithm, I decided to use thunks. Since Javascript functions are closures, these serve nicely as streams of values. This fits well with lazy functional programming, where an often-used technique is to create a lazy list of all possible results, then pop values off it until an acceptable one is found. This is exactly the case with an optimisation stream; we can keep pulling values out.

To create more and more elaborate (and hopefully powerful) algorithms, the library contains a wealth of higher-order functions which act as stream combinators. A stream combinator is a function which takes one or more streams or other values as arguments, and returns a new stream. For example, there is an `interleave` combinator which takes an array of streams and returns a stream which interleaves the values of each. In fact, the `interleave` combinator is a specialisation of the more general `chooser` combinator. `chooser` takes a stream of integers (`choices`) and an array of streams. It returns a stream of values taken from the given streams, where the stream is chosen by index, based on the next choice value. For example, if we use a stream of all zeros (which we can obtain by calling `zeros()`) as our choices then we would keep getting values from the first stream in the array.

`interleave` is simply `chooser` where the choice is fixed to increment each time (chooser applies `% streams.length` to its choices for us). To see how simple this is, let's see the code:

```javascript
var chooser = c(function(choices, streams) {
    // Interleaves N streams according to a stream of
    // integer choices from 0 to N-1
    return map(
        subscript(streams),
        map(
            modulus(streams.length),
            choices
        )
    );
});

var interleave = chooser(counter());
```

As you can see, this is written in a very functional style. There are some auxiliary functions being used here which I should probably explain:

The `c` function, which chooser's definition is wrapped in, is the Curry function which I explained in an earlier blog post. It allows us to supply a function's arguments a few at a time. All multi-argument functions in the search streams library are wrapped in `c` to make them Curryable. Notable uses of Currying here are the fact that `interleave` only supplies one argument to `chooser` (since, as we said above, it simply fixes the choice stream).

We also only give one argument to the binary functions `subscript` (which returns `first argument[second argument]`) and `modulus` (which returns `first argument % second argument`).

The `map` combinator sends the results of a stream through a function.

Finally `counter()` gives us a stream of the Natural numbers (0, 1, 2, ...).

As you can see, the nice property of combinators is that they compose nicely (as you would hope!). The result of one combinator can be passed as an argument to another. Indeed, the chain of `map -> chooser -> interleave` doesn't stop there. There are combinators which specialise the interleave combinator; notably the `exhaustive` combinator, which interleaves its argument with a brute-force enumeration. By wrapping a stream in `exhaustive`, we may at worst halve its rate of convergence, but we guarantee that a solution will eventually be found, if there is one in the domain (since the brute-force enumeration will eventually find one).

The code is available, as always, in [Git] [1] and is Public Domain. I plan to show off some examples here at some point, but given my track record for abandoning code we'll just have to see ;)

[1]: /git/search-optimisation-streams
