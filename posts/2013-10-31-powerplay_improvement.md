---
title: PowerPlay improvement
---
Recently I've been working on an implementation of [Prof. Jürgen Schmidhuber's](http://www.idsia.ch/~juergen/) [PowerPlay](http://www.idsia.ch/~juergen/interest.html) architecture, which is basically a greedy version of the [Gödel Machine](http://www.idsia.ch/~juergen/goedelmachine.html).

I've been using the [Coq](http://coq.inria.fr/) language, which is both a programming language based on [lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus) and a formal logic based on [Martin-Löf type theory](http://en.wikipedia.org/wiki/Intuitionistic_type_theory). My aim is to re-use as much of Coq's logic machinery as possible for the PowerPlay agent to do theorem proving about itself. As a bonus I would also like to end up with a [formally verified implementation](http://en.wikipedia.org/wiki/Formal_verification).

I've been approaching this from two sides:

 1) Creating the reflection machinery to allow Coq to reason about Coq.
 1) Creating an abstract specification of PowerPlay which I can check my eventual implementation against.

One issue with the reflection machinery is that Coq's [totality](http://en.wikipedia.org/wiki/Total_functional_programming) forces us to cut off our searches at some point to avoid becoming undecidable. I've decided to implement totality by restricting the amount of recursion allowed. This means we can't reason about Coq from within Coq, but we can reason about 'Coq with N levels of recursion' from within 'Coq with N+1 levels of recursion'. Being restricted to some predetermined choice of N can be avoided thanks to PowerPlay's recursive nature; on each iteration we can double N, easily making up for any initial choice.

Writing the specification has been quite interesting. My approach is to define a non-recursive PowerPlay variant which takes a problem domain as an argument, then turn it into a recursive variant by using self-improvement as the domain.

I wrote the code for both of these but my original definition of self-improvement seems to be quite strong and may have been impossible for an implementation to satisfy. I'm now trying to weaken this definition and at the same time make the specifications more abstract and modular so that many variants can be built by mixing and matching different parts (the original specification variations were hard-coded copypasta).

An interesting thing happens when we abstract over the self-improvement criterion. First we'll define some terms:

```ocaml
(* Problem is a Type, but it's definition is left as a parameter *)
Parameter Problem  : Type.
(* Solution builds Types out of Problems, again it is a parameter *)
Parameter Solution : Problem -> Type.

(* Solvers turn a Problem and a 'timeout' (Natural number) into an 'optional' Solution to that Problem.
   'Optional' means we can return "None" instead of a Solution if we want. *)
Definition Solver := forall (p : Problem) (n : nat),
                            option (Solution p).
```

In the original PowerPlay definition, we end up iterating through `States`{.ocaml} which have the following type:

```ocaml
Inductive State : Type := mkState :
         (* We can build a State out of a Solver 's' *)
  forall (s : Solver)
         (* And a list 'l' of (Problem, timeout) pairs *)
         (l : list (Problem * nat))
         (* And a proof 'f' that for all 'p' and 'n'... *)
         (f : forall p n,
                     (* If (p, n) is in l then 's p n' is not None *)
                     In (p, n) l -> s p n <> None),
         State.
```

This is actually badly specified. Firstly, by explicitly mentioning `list`{.ocaml}s we're forcing an implementation detail. Secondly, `list`{.ocaml}s don't actually have the 'elements must be solvable' property that we care about, so we're forced to keep track of that separately in our `State`{.ocaml}.

We can improve this by abstracting over the implementation details and allowing anything which has a suitable `In`{.ocaml} predicate and 'elements must be solvable' proof:

```ocaml
(* 'SolvableBy s' is our collection of Problems. This could be a list, but doesn't have to be. *)
Parameter SolvableBy : Solver -> Type.

(* 'In (p, n) sb' is a membership predicate for SolvableBy *)
Parameter In {s} : (Problem * nat) -> SolvableBy s -> Prop.

(* Implementors must prove that 'In (p, n) sb' implies that sb's Solver can solve p in timeout n. *)
Axiom solvable {s} {sb : SolvableBy s} p n : In (p, n) sb -> s p n <> None.
```

Now we can make a more abstract, more modular definition of `State`{.ocaml}:

```ocaml
Inductive State : Type := mkState :
         (* We can build a State out of a Solver 's' *)
  forall (s : Solver)
         (* And a collection of solvable problems *)
         (sb : SolvableBy s),
         State.
```

Now, assuming we've written a few trivial projection functions, we can define what it means for one `State`{.ocaml} to be an `Improvement`{.ocaml} of another:

```ocaml
Inductive Improvement (old_s : State) : Type := mkI :
         (* To make an Improvement we need a new State *)
  forall (new_s : State)
         (* We need a proof that everything in the old collection is in the new collection *)
         (in_p : forall pair,
                        In pair (solved_by old_s) -> In pair (solved_by new_s))
         (* We need a proof that there's a Problem in the new collection that's unsolvable by the old Solver *)
         (prob : exists p n,
                 In (p, n) (solved_by new_s) /\ (solver old_s) p n = None),
         Improvement old_s.
```

This definition is a straightforward formalisation of the original PowerPlay's improvement criterion. We can get back an exact implementation of PowerPlay by implementing `list`{.ocaml}s which only contain provably-solvable problems (this is the usual inductive definition of `list`{.ocaml}s, but with an argument for cons which proves solvability):

```ocaml
Inductive SolvableBy (s : Solver) : Type :=
           (* We can always construct the empty list nil *)
  sbNil  : SolvableBy s
                  (* Cons takes a Problem and timeout to prepend *)
  sbCons : forall p n,
                  (* It takes an existing list to prepend to *)
                  SolvableBy s  ->
                  (* It takes a proof that s can solve p in timeout n *)
                  s p n <> None ->
                  (* We get back a list with the pair prepended *)
                  SolvableBy s.

(* Projection functions *)
Definition head {s} (sb : SolvableBy s) : option (Problem * nat) :=
  match sb with
      | sbNil          => None
      | sbCons p n _ _ => Some (p, n)
  end.

Definition tail {s} (sb : SolvableBy s) : option (SolvableBy s) :=
  match sb with
      | sbNil => None
      | sbCons _ _ t _ => Some t
  end.

(* We can define In just like for lists *)
Inductive In {s : Solver} (pair : Problem * nat) (sb : SolvableBy s) : Prop :=
             (* An element is in the list if we can prove it's the head *)
  | isHead : (head sb = Some pair) -> In pair sb
             (* An element is in a list if it's in the tail *)
  | inTail : forall t, (tail sb = Some t) -> In pair t -> In pair sb.

(* We can trivially prove that our elements are solvable; we just need to look up the proof that was given when that element was prepended to the list *)
Fixpoint solvable {s} {sb : SolvableBy s} p n (i : In (p, n) sb) : s p n <> None :=
  match (i, sb) with
      | isHead _, sbCons _ _ _ proof => proof
      | inTail _ _ in_tail, sbCons _ _ t _ => solvable p n in_tail
  end.
```

That definition of `solvable`{.ocaml} may not work as-is, but it should go through with a few nudges in Coq's proof mode.

Now that we've generalised our collection, we don't need a to use `list`{.ocaml}s and explicit per-`Problem`{.ocaml} proofs like this. Instead, we can define our collection to be 'every `solvable`{.ocaml} `(Problem, timeout)`{.ocaml} pair', like this:

```ocaml
(* We don't need to store anything in 'SolvableBy s', since all we care about is 's'. "True" is Coq's unit type. *)
Definition SolvableBy (s : Solver) : Type := True.

(* "In pair sb" just tests whether the Problem's solvable *)
Definition In {s : Solver} (pair : Problem * nat) (sb : SolvableBy s) : Prop :=
  s (fst pair) (snd pair) <> None.

(* Our 'solvable' proof is trivial, since it's the definition of 'In' *)
Theorem solvable {s} {sb : SolvableBy s} p n (i : In (p, n) sb) : s p n <> None := i.
```

Again, we may have to fiddle `solvable`{.ocaml} in proof mode to convince Coq that it's correct, but it should go through easily enough.

By making this change to PowerPlay, we gain the following:

 - We get rid of the `list`{.ocaml} of `Problem`{.ocaml}s, which grows linearly with the number of improvements, replacing it with on-the-fly proofs derived straight from the `Solver`{.ocaml}s themselves.
 - The original PowerPlay definition guarantees that it won't lose the ability to solve the `Problem`{.ocaml}s in its `list`{.ocaml}; this new variant guarantees that it will never lose the ability to solve any `Problem`{.ocaml} ever.
 - The code ends up being simpler!

This work is still incomplete, but it's living at [https://gitorious.org/powerplay](https://gitorious.org/powerplay) for anyone who's interested.
