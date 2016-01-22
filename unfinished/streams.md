At the moment, [the PowerPlay variant I've been working on](/essays/powerplay) is completely pure, ie. its output depends only on its input. No useful problem-solver (or anything else for that matter!) can be pure, since we wouldn't be able to give it any problems to solve unless we restart it, which would throw away any improvements it's made!

Of course, like any purely functional program, there are various solutions to this such as [applicative functors](https://wiki.haskell.org/Applicative_functor), [monads](http://www.haskell.org/haskellwiki/Monad_tutorials_timeline), [arrows](https://www.haskell.org/arrows/) and [algebraic effect systems](http://lambda-the-ultimate.org/node/4481). These are all pretty heavyweight, so as an illustrative example I thought I'd go old school and demonstrate how to solve problems using stream-based IO (similar to that found in [Haskell 1.0](http://www.haskell.org/haskellwiki/Language_and_library_specification)).

Since this is just a demonstration, we'll use an incredibly naive implementation which works on one `Problem`{.ocaml} at a time until it's solved. First we need to define the streams; we currently have a `NoWorseStream`{.ocaml} which contains `Solvers`{.ocaml} which turn a `p : Problem`{.ocaml} into a `Solution p`{.ocaml}

```ocaml
(* An infinite stream containing 'Some Problem' and/or 'None' *)
CoInductive ProblemStream {D : Domain} : Type
  := (* Prepend a Problem *)
     probCons : Problem -> ProblemStream -> ProblemStream

     (* Don't prepend anything, equivalent to prepending None *)
   | probNone : ProblemStream -> ProblemStream.

(* An infinite stream containing Solutions to a given ProblemStream *)
CoInductive SolutionStream {D : Domain} (ps : ProblemStream) : Type :=
  := (* Prepend a Solution to a Problem *)
     solCons : forall {p ps'},
                      (* If ps is made out of p and ps' *)
                      ps = probCons p ps' ->
                      (* and we have a Solution for p *)
                      Solution p                 ->
                      (* and we have a SolutionStream for ps' *)
                      SolutionStream ps'         ->
                      (* then we can make a SolutionStream for ps *)
                      SolutionStream ps

     (* Don't prepend anything, equivalent to prepending None *)
   | solNone : SolutionStream ps -> SolutionStream ps.

(* Combines a NoWorseStream and a SolutionStream *)
CoInductive NWSolStream {D  : Domain}
                        (ps : ProblemStream)
                        (s  : Solver)
                            : Type
     (* We have an  *)
  := nwsolCons


CoFixpoint powerplay'' (D   : Domain)
                       (L   : Lang)
                       (G   : GivenSearcher)
                       (ps  : ProblemStream)
                       (ast : AST)
                            : SolutionStream ps
  := (* Try to improve ourselves *)
     let found := searcher' ast (2 ^ n) in
     let ast'  := projT1 found          in
     let proof := projT2 found          in
     let iAst  := interpret ast         in
     let iAst' := interpret ast'        in
     (* See if we've been given a Problem *)
     match ps with
         (* No Problem, just recurse *)
       | probNone   ps' => nwsolNone iAst
                                     iAst'
                                     proof
                                     (powerplay'' D L G ps' ast')

         (* We have a Problem, can we solve it? *)
       | probCons p ps' => match interpret ast' p with
           (* Yes *)
         | Some s => nwsolCons iAst
                               iAst'
                               proof
                               s
                               (powerplay'' D L G ps' ast')

           (* No *)
         | None   => nwsolNone iAst
                               iAst'
                               proof
                               (powerplay'' D L G ps ast')
       end
     end.

let found := searcher' ast (2 ^ n) in
let ast'  := projT1 found  in
let proof := projT2 found  in
    nwsCons  iAst
             iAst'
             proof
             (powerplay' ast' (S n)).

```
