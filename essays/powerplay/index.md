---
title: PowerPlay: A General Learning Framework
---

[PowerPlay](http://www.idsia.ch/~juergen/interest.html) is a very simple, very general architecture for an AI agent. It uses:

 - Self-modification
 - Auto-generated problems
 - Some decidable notion of "regression"

With these at its disposal, the algorithm is very simple:

> Search for, then perform, a self-modification which:
>  1. Is not a regression
>  2. Allows us to solve a problem we previously couldn't

## PowerPlay ##

In the original definition of PowerPlay by [Jürgen Schmidhuber](http://www.idsia.ch/~juergen), the agent keeps a list of the previously-unsolvable problem used to justify (2), and uses the solvability of this list as its regression check.

This is a very weak notion of regression, and maintaining the list has linear memory cost.

## Generalisations ##

Other regression mechanisms are possible, especially in the presence of dependent types.

For example, we can model our agent's current problem-solver using the following types:

```ocaml
Parameter  Problem  :  Type.
Parameter  Solution :  Problem -> Type.
Definition Solver   := forall (p : Problem), option (Solution p).
```

This leads to a natural, symmetric definition of valid self-modifications:

```ocaml
Definition Solves (s : Solver) (p : Problem) := s p <> None.

Definition AsGoodAs (s1 s2 : Solver) := forall p, Solves s2 p -> Solves s1 p.

Definition Improvement (s : Solver) (m : Mod) := let s' := apply s m in
                                                 AsGoodAs s' s /\ ~(AsGoodAs s s')
```

These two `AsGoodAs`{.ocaml} propositions implement the two conditions of an acceptable modification.

Note that, since we derive the regression checks from the Solvers, this approach doesn't require any memory overhead. It also provides a much stronger guarantee: no solvable Problem can ever become unsolvable.

I believe this is more desirable than the weak guarantee of the original PowerPlay, although implementations need to be aware of this strength, for example if they want to include resource bounds in their Problems.

## More Information ##

I've [implemented these ideas in Coq](https://www.gitorious.org/powerplay) and there is further explanation in these blog posts:

 - [PowerPlay Improvement](/posts/2013-10-31-powerplay_improvement.html)
 - [PowerPlay Improvement Continued](/posts/2013-11-22-powerplay_improvement_continued.html)
 - [PowerPlay Implementations](/posts/2013-11-20-powerplay_implementations.html)
